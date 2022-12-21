use anyhow::bail;

use formality_types::{
    cast::{To, Upcast},
    derive_links::{Parameter, Variable},
    grammar::{
        EnsuresTy, Fallible, Goal, ImplicationTy, PredicateTy, RigidName, RigidTy, Ty, TyData,
        Variance,
    },
    seq,
};

use super::{extrude::Relationship, Env};
use crate::Db;

impl Env {
    /// Require `a <: b`, yielding a new environment + list of goals that must all be solved for `a <: b` to be true.
    /// Returns `Err` if the two parameters can never be related.
    pub(super) fn sub(&self, db: &Db, a: &Parameter, b: &Parameter) -> Fallible<(Env, Vec<Goal>)> {
        let mut env = self.clone();
        let a = env.refresh_inference_variables(a);
        let b = env.refresh_inference_variables(b);
        let goals = env.sub_parameters(db, &a, &b)?;
        Ok((env, goals))
    }

    fn sub_parameters(&mut self, db: &Db, a: &Parameter, b: &Parameter) -> Fallible<Vec<Goal>> {
        match (&a, &b) {
            (Parameter::Ty(a), Parameter::Ty(b)) => self.sub_tys(db, a, b),
            (Parameter::Lt(_), Parameter::Lt(_)) => Ok(vec![Goal::outlives(a, b)]),
            (Parameter::Ty(_), _) | (Parameter::Lt(_), _) => panic!("ill-kinded: {a:?} vs {b:?}"),
        }
    }

    fn sub_tys(&mut self, db: &Db, a: &Ty, b: &Ty) -> Fallible<Vec<Goal>> {
        if a == b {
            return Ok(vec![]);
        }

        match (a.data(), b.data()) {
            // Bound variables should always be instantiated by the time we get here.
            (TyData::Variable(Variable::BoundVar(_)), _)
            | (_, TyData::Variable(Variable::BoundVar(_))) => {
                panic!("found unexpected bound variable")
            }

            // The only way to be a subtype of a rigid type is to be another instance
            // of that rigid type (but we may have distinct parameters). Therefore,
            // given `?a <: b<...>`, we can instantiate `?a` as a version of `b`.
            (
                &TyData::Variable(Variable::InferenceVar(a)),
                TyData::RigidTy(RigidTy {
                    name,
                    parameters: parameters_b,
                }),
            ) => {
                assert!(!self.is_mapped(a));
                let a1 = self.fresh_rigid_ty(db, name, self.data(a).universe);
                let equate_goals = self.equate_var(a, &a1)?;
                let subty_goals = self.sub_rigid_tys(db, name, &a1.parameters, parameters_b);
                Ok(seq![
                    a1.to::<Ty>().well_formed().upcast(),
                    ..equate_goals,
                    ..subty_goals
                ])
            }

            // As in the previous rule, but reversed.
            (
                TyData::RigidTy(RigidTy {
                    name,
                    parameters: parameters_a,
                    ..
                }),
                &TyData::Variable(Variable::InferenceVar(b)),
            ) => {
                assert!(!self.is_mapped(b));
                let b1 = self.fresh_rigid_ty(db, name, self.data(b).universe);
                let equate_goals = self.equate_var(b, &b1)?;
                let subty_goals = self.sub_rigid_tys(db, name, parameters_a, &b1.parameters);
                Ok(seq![
                    b1.to::<Ty>().well_formed().upcast(),
                    ..equate_goals,
                    ..subty_goals,
                ])
            }

            (
                &TyData::Variable(Variable::InferenceVar(var_a)),
                &TyData::Variable(Variable::InferenceVar(var_b)),
            ) => {
                if self.data(var_a).universe < self.data(var_b).universe {
                    Ok(self.relate_parameter(var_b, Relationship::SupertypeOf, a))
                } else {
                    Ok(self.relate_parameter(var_a, Relationship::SubtypeOf, b))
                }
            }

            // The only way to relate to a placeholder variable is to be equal to it.
            // This is an optimization of sorts.
            (
                TyData::Variable(Variable::PlaceholderVar(_)),
                TyData::Variable(Variable::InferenceVar(_)),
            )
            | (
                TyData::Variable(Variable::InferenceVar(_)),
                TyData::Variable(Variable::PlaceholderVar(_)),
            ) => Ok(vec![Goal::eq(a, b)]),

            // Relating two rigid types: use variance.
            (
                TyData::RigidTy(RigidTy {
                    name: name_a,
                    parameters: parameters_a,
                }),
                TyData::RigidTy(RigidTy {
                    name: name_b,
                    parameters: parameters_b,
                }),
            ) => {
                if name_a != name_b {
                    anyhow::bail!(
                        "no subtyping relationship between `{name_a:?}` and `{name_b:?}`"
                    );
                }

                Ok(self.sub_rigid_tys(db, name_a, parameters_a, parameters_b))
            }

            // Two alias types can be a subtype if...
            // * they normalize to subtypes
            // * their parametes are equal, presuming they are the same alias
            (TyData::AliasTy(alias_a), TyData::AliasTy(alias_b)) => {
                let normalizes_goal = Goal::exists_f(|(ty_a, ty_b): (Ty, Ty)| {
                    Goal::all(vec![
                        alias_a.normalizes_to(&ty_a).upcast(),
                        alias_b.normalizes_to(&ty_b).upcast(),
                        Goal::sub(ty_a, ty_b),
                    ])
                });

                if alias_a.name == alias_b.name {
                    Ok(vec![Goal::any(vec![
                        normalizes_goal,
                        Goal::all(zip_eq(&alias_a.parameters, &alias_b.parameters)),
                    ])])
                } else {
                    Ok(vec![normalizes_goal])
                }
            }

            // An alias `A <: T` if either `A == T` or we can normalize the alias
            // to `A'` and `A' <: T`.
            (TyData::AliasTy(alias_a), TyData::Variable(Variable::InferenceVar(_))) => {
                let alternatives = Goal::any(seq![
                    Goal::eq(a, b),
                    Goal::exists_f(|a1: Ty| Goal::all(seq![
                        alias_a.normalizes_to(&a1).upcast(),
                        Goal::sub(a1, b),
                    ]))
                ]);
                Ok(seq![alternatives])
            }
            (TyData::Variable(Variable::InferenceVar(_)), TyData::AliasTy(alias_b)) => {
                let alternatives = Goal::any(seq![
                    Goal::eq(a, b),
                    Goal::exists_f(|b1: Ty| Goal::all(seq![
                        alias_b.normalizes_to(&b1).upcast(),
                        Goal::sub(a, b1),
                    ]))
                ]);
                Ok(seq![alternatives])
            }

            // Overly conservative rule, but this is what the compiler does.
            //
            // FIXME. This is where simplesub would do something different.
            // Could also use simplesub to change how alias types are handled
            // here.
            (&TyData::Variable(Variable::InferenceVar(var_a)), TyData::PredicateTy(_)) => {
                self.equate_var(var_a, b)
            }
            (TyData::PredicateTy(_), &TyData::Variable(Variable::InferenceVar(var_b))) => {
                self.equate_var(var_b, a)
            }

            // Otherwise, an alias is a subtype of T if we can normalize to some subtype of T.
            (TyData::AliasTy(alias_a), TyData::PredicateTy(_))
            | (TyData::AliasTy(alias_a), TyData::RigidTy(_))
            | (TyData::AliasTy(alias_a), TyData::Variable(Variable::PlaceholderVar(_))) => {
                let normalizes_goal = Goal::exists_f(|ty: Ty| {
                    Goal::all(vec![alias_a.normalizes_to(&ty).upcast(), Goal::sub(ty, b)])
                });
                Ok(vec![normalizes_goal])
            }

            // As in the previous rule, but reversed.
            (TyData::PredicateTy(_), TyData::AliasTy(alias_b))
            | (TyData::RigidTy(_), TyData::AliasTy(alias_b))
            | (TyData::Variable(Variable::PlaceholderVar(_)), TyData::AliasTy(alias_b)) => {
                let normalizes_goal = Goal::exists_f(|ty: Ty| {
                    Goal::all(vec![alias_b.normalizes_to(&ty).upcast(), Goal::sub(a, ty)])
                });
                Ok(vec![normalizes_goal])
            }

            // Placeholder variables are not equal to rigid types or other placeholders.
            (TyData::Variable(Variable::PlaceholderVar(_)), TyData::RigidTy(_))
            | (TyData::RigidTy(_), TyData::Variable(Variable::PlaceholderVar(_)))
            | (
                TyData::Variable(Variable::PlaceholderVar(_)),
                TyData::Variable(Variable::PlaceholderVar(_)),
            ) => {
                assert_ne!(a, b);
                bail!("not-eq({a:?}, {b:?})")
            }

            (_, TyData::PredicateTy(PredicateTy::ForAll(binder))) => {
                let b1 = self.instantiate_universally(binder);
                Ok(vec![Goal::sub(a, b1)])
            }

            (TyData::PredicateTy(PredicateTy::ForAll(binder)), _) => {
                let a1 = self.instantiate_existentially(binder);
                Ok(vec![Goal::sub(a1, b)])
            }

            // FIXME: Can't actually permit existentials in this open-ended way. e.g., `&u32` is not a subtype of `&dyn Foo`.
            (TyData::PredicateTy(PredicateTy::Exists(binder)), _) => {
                let a1 = self.instantiate_universally(binder);
                Ok(vec![Goal::sub(a1, b)])
            }

            // FIXME: Can't actually permit existentials in this open-ended way. e.g., `&u32` is not a subtype of `&dyn Foo`.
            (_, TyData::PredicateTy(PredicateTy::Exists(binder))) => {
                let b1 = self.instantiate_existentially(binder);
                Ok(vec![Goal::sub(a, b1)])
            }

            (
                _,
                TyData::PredicateTy(PredicateTy::ImplicationTy(ImplicationTy {
                    predicates,
                    ty: b1,
                })),
            ) => Ok(vec![Goal::implies(predicates, Goal::sub(a, b1))]),

            (
                TyData::PredicateTy(PredicateTy::ImplicationTy(ImplicationTy {
                    predicates,
                    ty: a1,
                })),
                _,
            ) => Ok(vec![Goal::all((predicates, vec![Goal::sub(a1, b)]))]),

            (_, TyData::PredicateTy(PredicateTy::EnsuresTy(EnsuresTy { ty: b1, predicates }))) => {
                Ok(vec![Goal::implies(predicates, Goal::sub(a, b1))])
            }

            (TyData::PredicateTy(PredicateTy::EnsuresTy(EnsuresTy { ty: a1, predicates })), _) => {
                Ok(vec![Goal::all((predicates, vec![Goal::sub(a1, b)]))])
            }
        }
    }

    fn sub_rigid_tys(
        &mut self,
        db: &Db,
        name: &RigidName,
        parameters_a: &[Parameter],
        parameters_b: &[Parameter],
    ) -> Vec<Goal> {
        let generics = self.rigid_generics(db, name);
        assert_eq!(parameters_a.len(), generics.len());
        assert_eq!(parameters_b.len(), generics.len());

        parameters_a
            .iter()
            .zip(parameters_b)
            .zip(&generics)
            .map(|((p_a, p_b), (_, variance))| match variance {
                Variance::Covariant => Goal::sub(p_a, p_b),
                Variance::Contravariant => Goal::sub(p_b, p_a),
                Variance::Invariant => Goal::eq(p_a, p_b),
            })
            .collect()
    }
}

fn zip_eq(a_s: &[Parameter], b_s: &[Parameter]) -> Vec<Goal> {
    a_s.iter().zip(b_s).map(|(a, b)| Goal::eq(a, b)).collect()
}
