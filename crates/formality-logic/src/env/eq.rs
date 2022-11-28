use anyhow::bail;

use contracts::requires;
use formality_types::{
    cast::Upcast,
    derive_links::{Parameter, Variable},
    grammar::{Fallible, Goal, InferenceVar, Lt, LtData, RigidTy, Ty, TyData},
    seq,
};

use super::Env;
use crate::Db;

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
            // Bound variables should always be instantiated by the time we get here.
            (TyData::Variable(Variable::BoundVar(_)), _)
            | (_, TyData::Variable(Variable::BoundVar(_))) => {
                panic!("found unexpected bound variable")
            }

            // ?X == !Y where !Y is in a universe ?X cannot name: unprovable.
            (
                &TyData::Variable(Variable::InferenceVar(i)),
                TyData::Variable(Variable::PlaceholderVar(p)),
            )
            | (
                TyData::Variable(Variable::PlaceholderVar(p)),
                &TyData::Variable(Variable::InferenceVar(i)),
            ) if self.data(i).universe < p.universe => {
                bail!("cannot equate inference variable `{i:?}` with placeholder `{p:?}`")
            }

            // ?X == ?Y, where ?X and ?Y are in distinct universes:
            //
            // Map the variable in the higher universe to the other.
            (
                &TyData::Variable(Variable::InferenceVar(i)),
                &TyData::Variable(Variable::InferenceVar(j)),
            ) if self.data(i).universe < self.data(j).universe => self.equate_var(j, a),
            (
                &TyData::Variable(Variable::InferenceVar(i)),
                &TyData::Variable(Variable::InferenceVar(j)),
            ) if self.data(j).universe < self.data(i).universe => self.equate_var(i, b),

            // (?X == T) or (T == ?X): Map ?X to T.
            //
            // Subtle: if `b` is a variable in a universe unsuitable for `a`,
            // this would simply result in a goal that `a == b`, which would
            // yield an infinite. This is why we handle those cases above.
            (&TyData::Variable(Variable::InferenceVar(a)), _) => self.equate_var(a, b),
            (_, &TyData::Variable(Variable::InferenceVar(b))) => self.equate_var(b, a),

            // Two aliases must have same name/parameters *or* normalize to the same thing.
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

            // An alias type is equal to another type if it normalizes to that type.
            (TyData::AliasTy(alias_a), _) => Ok(vec![alias_a.normalizes_to(b).upcast()]),
            (_, TyData::AliasTy(alias_b)) => Ok(vec![alias_b.normalizes_to(a).upcast()]),

            // Rigid types are equal if their parameters are equal (regardless of variance).
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

            // Placeholders are not equal to rigid types or other placeholders.
            (TyData::Variable(Variable::PlaceholderVar(_)), TyData::RigidTy(_))
            | (
                TyData::Variable(Variable::PlaceholderVar(_)),
                TyData::Variable(Variable::PlaceholderVar(_)),
            )
            | (TyData::RigidTy(_), TyData::Variable(Variable::PlaceholderVar(_))) => {
                bail!("not-eq({a:?}, {b:?})")
            }

            // Predicate types can only be managed through subtyping.
            (TyData::PredicateTy(_), _) | (_, TyData::PredicateTy(_)) => {
                // FIXME: Predicate types do not have canonical forms in this setup, but that mixes poorly with `TypeId`.
                Ok(vec![Goal::sub(a, b), Goal::sub(b, a)])
            }
        }
    }

    fn eq_lts(&mut self, _db: &Db, a: &Lt, b: &Lt) -> Fallible<Vec<Goal>> {
        if a == b {
            return Ok(vec![]);
        }

        match (a.data(), b.data()) {
            (LtData::Static, LtData::Static) => Ok(vec![]),

            // Map inference variables if we can.
            (&LtData::Variable(Variable::InferenceVar(a)), _)
                if self.data(a).universe >= self.term_universe(b) =>
            {
                self.equate_var(a, b)
            }
            (_, &LtData::Variable(Variable::InferenceVar(b)))
                if self.data(b).universe >= self.term_universe(a) =>
            {
                self.equate_var(b, a)
            }

            // Otherwise convert to outlives relationships.
            _ => Ok(vec![Goal::outlives(a, b), Goal::outlives(b, a)]),
        }
    }

    #[requires(!self.is_mapped(variable))]
    pub(super) fn equate_var(
        &mut self,
        variable: InferenceVar,
        value: &impl Upcast<Parameter>,
    ) -> Fallible<Vec<Goal>> {
        let value: &Parameter = &value.upcast();

        if self.occurs_in(variable, value) {
            bail!("unifying `{variable:?}` and `{value:?}` would create a cyclic type");
        }

        let variable_universe = self.data(variable).universe;
        if self.term_universe(value) <= variable_universe {
            Ok(self.map_to(variable, value))
        } else {
            let value_u = self.generalize_universe(variable_universe, value);
            let map_to_goals = self.map_to(variable, &value_u);
            Ok(seq![..map_to_goals, Goal::eq(value_u, value)])
        }
    }
}

fn zip_eq(a_s: &[Parameter], b_s: &[Parameter]) -> Vec<Goal> {
    a_s.iter().zip(b_s).map(|(a, b)| Goal::eq(a, b)).collect()
}
