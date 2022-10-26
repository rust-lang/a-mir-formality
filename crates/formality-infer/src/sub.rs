use anyhow::bail;

use formality_types::{
    cast::Upcast,
    db::Db,
    derive_links::{Parameter, Variable},
    grammar::{Fallible, Goal, RigidTy, Ty, TyData},
};

use super::Env;

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
            (
                &TyData::Variable(Variable::InferenceVar(a)),
                TyData::RigidTy(RigidTy { name, .. }),
            ) => {
                assert!(!self.is_mapped(a));
                let a1: Ty = self.fresh_rigid_ty(db, name, self.data(a).universe);
                let mut goals = vec![a1.well_formed().upcast()];
                goals.extend(self.map_to(a, &a1)?);
                goals.push(Goal::sub(a1, b));
                Ok(goals)
            }

            (
                TyData::RigidTy(RigidTy { name, .. }),
                &TyData::Variable(Variable::InferenceVar(b)),
            ) => {
                assert!(!self.is_mapped(b));
                let b1: Ty = self.fresh_rigid_ty(db, name, self.data(b).universe);
                let mut goals = vec![b1.well_formed().upcast()];
                goals.extend(self.map_to(b, &b1)?);
                goals.push(Goal::sub(a, b1));
                Ok(goals)
            }

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
                    anyhow::bail!("cannot equate `{a:?}` and `{b:?}`");
                }

                // FIXME: variance

                Ok(zip_eq(parameters_a, parameters_b))
            }

            (&TyData::Variable(Variable::InferenceVar(a)), _) => {
                assert!(!self.is_mapped(a));
                let goals = self.map_to(a, b)?;
                Ok(goals)
            }

            (_, &TyData::Variable(Variable::InferenceVar(b))) => {
                assert!(!self.is_mapped(b));
                let goals = self.map_to(b, a)?;
                Ok(goals)
            }

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

            (TyData::AliasTy(alias_a), _) => {
                let normalizes_goal = Goal::exists_f(|ty: Ty| {
                    Goal::all(vec![alias_a.normalizes_to(&ty).upcast(), Goal::sub(ty, b)])
                });
                Ok(vec![normalizes_goal])
            }

            (_, TyData::AliasTy(alias_b)) => {
                let normalizes_goal = Goal::exists_f(|ty: Ty| {
                    Goal::all(vec![alias_b.normalizes_to(&ty).upcast(), Goal::sub(a, ty)])
                });
                Ok(vec![normalizes_goal])
            }

            (TyData::Variable(Variable::PlaceholderVar(_)), _)
            | (_, TyData::Variable(Variable::PlaceholderVar(_))) => {
                bail!("not-eq({a:?}, {b:?})")
            }

            (TyData::Variable(Variable::BoundVar(_)), _)
            | (_, TyData::Variable(Variable::BoundVar(_))) => {
                panic!("found unexpected bound variable")
            }

            (TyData::PredicateTy(_), _) | (_, TyData::PredicateTy(_)) => todo!(),
        }
    }
}

fn zip_eq(a_s: &[Parameter], b_s: &[Parameter]) -> Vec<Goal> {
    a_s.iter().zip(b_s).map(|(a, b)| Goal::eq(a, b)).collect()
}
