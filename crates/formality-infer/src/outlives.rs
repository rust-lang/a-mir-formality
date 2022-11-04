use formality_types::{
    cast::{Upcast, Upcasted},
    db::Db,
    derive_links::Parameter,
    grammar::{
        EnsuresTy, Fallible, Goal, Hypothesis, ImplicationTy, LtData, ParameterData, PredicateTy,
        RigidName, RigidTy, Ty, TyData, Variable,
    },
    seq,
};

use super::Env;

impl Env {
    /// Require `a : b`, yielding a new environment + list of goals that must all be solved for `a : b` to be true.
    /// Returns `Err` if the two parameters can never be related.
    pub(super) fn outlives(
        &self,
        db: &Db,
        a: &Parameter,
        b: &Parameter,
    ) -> Fallible<(Env, Vec<Goal>)> {
        let mut env = self.clone();
        let goals = env.outlives_parameters(db, a, b)?;
        Ok((env, goals))
    }

    fn outlives_parameters(
        &mut self,
        _db: &Db,
        a: &Parameter,
        b: &Parameter,
    ) -> Fallible<Vec<Goal>> {
        match (a.data(), b.data()) {
            (ParameterData::Lt(LtData::Static), _) => Ok(vec![]),

            // (&'a T : P) or (&'a mut T : P) if
            //     'a : P
            //
            // This is a hack / optimization on the more general case below that is based on knowing
            // that `&'a T` is only well-formed it `T : 'a`. The general rule would have us prove
            // that *both* `T : P` and `'a : P`. But if `T : 'a` and `'a : P`, then `T : P` is implied by
            // transitivity, so we can check check whether `'a : P` and that should be good enough.
            //
            // We could do this for arbitrary rigid types if we inspected their where-clauses,
            // but I didn't feel like writing up that logic just now, and the rule below is not wrong,
            // it just produces more answers than are necessary. (So, alternatively, we could also
            // work on the "answer subsumption" logic.)
            (
                ParameterData::Ty(TyData::RigidTy(RigidTy {
                    name: RigidName::Ref(_),
                    parameters,
                })),
                _,
            ) => {
                let lt = &parameters[0];
                Ok(seq![Goal::outlives(lt, b)])
            }

            // R<Pr_0...Pr_n> : Pb if ∀i (Pr_i : Pb)
            (ParameterData::Ty(TyData::RigidTy(r)), _) => {
                Ok(r.parameters.iter().map(|p| Goal::outlives(p, b)).collect())
            }

            // Pa : R<Pr_0...Pr_n> if ∃i (Pa : Pr_i)
            // Pa : R if Pa : static
            (_, ParameterData::Ty(TyData::RigidTy(r))) => {
                if r.parameters.len() >= 1 {
                    Ok(seq![Goal::any(
                        r.parameters
                            .iter()
                            .map(|p| Goal::outlives(a, p))
                            .collect::<Vec<_>>(),
                    )])
                } else {
                    Ok(seq![Goal::outlives(a, LtData::Static)])
                }
            }

            // A<Pa ...> : P if
            //      ∀i. (Pa_i : P) or
            //      (A<Pa ...> ~~~> T; T : P)
            //
            // To establish that an alias type outlives P we can either normalize the alias type or
            // we can relate P to each of the alias type's parameters. The latter is based on the reasoning
            // that the alias must be a function of its inputs and other static things.
            (ParameterData::Ty(TyData::AliasTy(a)), _) => {
                let parameters_goal = Goal::all(
                    a.parameters
                        .iter()
                        .map(|p| Goal::outlives(p, b))
                        .collect::<Vec<_>>(),
                );

                let alias_goal = Goal::exists_f(|t: Ty| {
                    Goal::all(vec![a.normalizes_to(&t).upcast(), Goal::outlives(&t, b)])
                });

                Ok(vec![Goal::any(vec![alias_goal, parameters_goal])])
            }

            // P : A<Pa ...> if
            //      (A<Pa ...> ~~~> T; P : T) or
            //      P : static
            //
            // To establish that P outlives an alias type we *must* normalize.
            // No matter what its arguments, the alias type could normalize to `i32` or some such thing,
            // in which case only static outlives it.
            (_, ParameterData::Ty(TyData::AliasTy(b))) => {
                let alias_goal = Goal::exists_f(|t: Ty| {
                    Goal::all(seq![b.normalizes_to(&t).upcast(), Goal::outlives(a, &t)])
                });

                Ok(seq![Goal::any(seq![
                    alias_goal,
                    Goal::outlives(a, LtData::Static),
                ])])
            }

            (ParameterData::Ty(TyData::PredicateTy(PredicateTy::Exists(a))), _) => {
                let (names, ty) = a.open();
                Ok(vec![Goal::for_all(&names, Goal::outlives(ty, b))])
            }

            (_, ParameterData::Ty(TyData::PredicateTy(PredicateTy::ForAll(b)))) => {
                let (names, ty) = b.open();
                Ok(vec![Goal::for_all(&names, Goal::outlives(a, ty))])
            }

            (ParameterData::Ty(TyData::PredicateTy(PredicateTy::ForAll(a))), _) => {
                let (names, ty) = a.open();
                Ok(vec![Goal::exists(&names, Goal::outlives(ty, b))])
            }

            (_, ParameterData::Ty(TyData::PredicateTy(PredicateTy::Exists(b)))) => {
                let (names, ty) = b.open();
                Ok(vec![Goal::exists(&names, Goal::outlives(a, ty))])
            }

            (
                _,
                ParameterData::Ty(TyData::PredicateTy(PredicateTy::EnsuresTy(EnsuresTy {
                    ty,
                    predicates,
                }))),
            ) => Ok(seq![Goal::implies(predicates, Goal::outlives(a, ty))]),

            (
                ParameterData::Ty(TyData::PredicateTy(PredicateTy::ImplicationTy(ImplicationTy {
                    predicates,
                    ty,
                }))),
                _,
            ) => Ok(seq![Goal::implies(predicates, Goal::outlives(ty, b))]),

            (
                ParameterData::Ty(TyData::PredicateTy(PredicateTy::EnsuresTy(EnsuresTy {
                    ty,
                    predicates,
                }))),
                _,
            ) => Ok(seq![Goal::outlives(ty, b), ..predicates.upcasted()]),

            (
                _,
                ParameterData::Ty(TyData::PredicateTy(PredicateTy::ImplicationTy(ImplicationTy {
                    predicates,
                    ty,
                }))),
            ) => Ok(seq![Goal::outlives(a, ty), ..predicates.upcasted()]),

            (ParameterData::Ty(TyData::Variable(Variable::BoundVar(_))), _)
            | (ParameterData::Lt(LtData::Variable(Variable::BoundVar(_))), _)
            | (_, ParameterData::Ty(TyData::Variable(Variable::BoundVar(_))))
            | (_, ParameterData::Lt(LtData::Variable(Variable::BoundVar(_)))) => {
                panic!("unexpected bound variable")
            }

            _ => todo!(),
        }
    }
}
