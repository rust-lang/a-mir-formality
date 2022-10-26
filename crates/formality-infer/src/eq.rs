use anyhow::bail;

use formality_types::{
    cast::Upcast,
    db::Db,
    derive_links::{Parameter, Variable},
    grammar::{Fallible, Goal, Lt, RigidTy, Ty, TyData},
};

use super::Env;

/// Equate two parameters, yielding a new environment + list of goals that must all be solved for the equate to be true.
/// Returns `Err` if the two parameters can never be proven equal.
impl Env {
    pub(super) fn eq(&self, db: &Db, a: &Parameter, b: &Parameter) -> Fallible<(Env, Vec<Goal>)> {
        let mut env = self.clone();
        let a = env.refresh_inference_variables(a);
        let b = env.refresh_inference_variables(b);
        let goals = env.eq_parameters(db, &a, &b)?;
        Ok((env, goals))
    }

    fn eq_parameters(&mut self, db: &Db, a: &Parameter, b: &Parameter) -> Fallible<Vec<Goal>> {
        match (&a, &b) {
            (Parameter::Ty(a), Parameter::Ty(b)) => self.eq_tys(db, a, b),
            (Parameter::Lt(a), Parameter::Lt(b)) => self.eq_lts(db, a, b),
            (Parameter::Ty(_), _) | (Parameter::Lt(_), _) => panic!("ill-kinded: {a:?} vs {b:?}"),
        }
    }

    fn eq_tys(&mut self, _db: &Db, a: &Ty, b: &Ty) -> Fallible<Vec<Goal>> {
        if a == b {
            return Ok(vec![]);
        }

        match (a.data(), b.data()) {
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
                let normalizes_goal = Goal::exists_f(|ty: Ty| {
                    Goal::all(vec![alias_a.normalizes_to(&ty), alias_b.normalizes_to(&ty)])
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

            (TyData::AliasTy(alias_a), _) => Ok(vec![alias_a.normalizes_to(b).upcast()]),

            (_, TyData::AliasTy(alias_b)) => Ok(vec![alias_b.normalizes_to(a).upcast()]),

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

                Ok(zip_eq(parameters_a, parameters_b))
            }

            (TyData::Variable(Variable::PlaceholderVar(_)), _)
            | (_, TyData::Variable(Variable::PlaceholderVar(_))) => {
                bail!("not-eq({a:?}, {b:?})")
            }

            (TyData::Variable(Variable::BoundVar(_)), _)
            | (_, TyData::Variable(Variable::BoundVar(_))) => {
                panic!("found unexpected bound variable")
            }

            (TyData::PredicateTy(_), _) | (_, TyData::PredicateTy(_)) => {
                // FIXME: Predicate types do not have canonical forms in this setup, but that mixes poorly with `TypeId`.
                Ok(vec![Goal::sub(a, b), Goal::sub(b, a)])
            }
        }
    }

    fn eq_lts(&mut self, _db: &Db, _a: &Lt, _b: &Lt) -> Fallible<Vec<Goal>> {
        unimplemented!("equate lifetimes")
    }
}

fn zip_eq(a_s: &[Parameter], b_s: &[Parameter]) -> Vec<Goal> {
    a_s.iter().zip(b_s).map(|(a, b)| Goal::eq(a, b)).collect()
}
