use std::sync::Arc;

use formality_types::{
    env::Env,
    grammar::{Goal, GoalData, Hypothesis},
};

use crate::elaborate_hypotheses;

/// Prove a "top-level" goal is true in the given environment
/// using the cosld solver. cosld is a basic [SLD] solving algorithm,
/// enriched to handle [FOHH] predicates as well as to
/// support a simple form of coinduction.
///
/// [SLD]: https://en.wikipedia.org/wiki/SLD_resolution
/// [FOHH]: https://en.wikipedia.org/wiki/Harrop_formula
pub fn cosld() {}

#[derive(Clone)]
struct CosldSolver {
    db: Arc<dyn crate::Db>,
    env: Env,
    hypotheses: Vec<Hypothesis>,
}

enum CosldResult {
    Yes(Env),
    Maybe,
}

impl CosldSolver {
    fn prove_goal(mut self, goal: &Goal) -> impl Iterator<Item = CosldResult> {
        match goal.data() {
            GoalData::AtomicPredicate(_) => todo!(),
            GoalData::AtomicRelation(_) => todo!(),
            GoalData::ForAll(binder) => {
                let subgoal = self.env.instantiate_universally(binder);
                self.prove_goal(&subgoal)
            }
            GoalData::Exists(binder) => {
                let subgoal = self.env.instantiate_existentially(binder);
                self.prove_goal(&subgoal)
            }
            GoalData::Implies(conditions, subgoal) => {
                self.with_hypotheses(conditions).prove_goal(subgoal)
            }
            GoalData::Any(subgoals) => subgoals
                .to_owned()
                .into_iter()
                .flat_map(move |subgoal| self.clone().prove_goal(&subgoal))
                .boxed(),
            GoalData::All(subgoals) => self.prove_all(subgoals).boxed(),
            GoalData::CoherenceMode(subgoal) => self
                .with_hypotheses(&[Hypothesis::coherence_mode()])
                .prove_goal(subgoal),
            GoalData::Ambiguous => std::iter::once(CosldResult::Maybe).boxed(),
        }
    }

    fn prove_all(self, _subgoals: &[Goal]) -> impl Iterator<Item = CosldResult> {
        std::iter::empty() // TODO
    }

    fn with_hypotheses(mut self, hypotheses: &[Hypothesis]) -> Self {
        self.hypotheses.extend(hypotheses.iter().cloned());
        self.hypotheses = elaborate_hypotheses::elaborate_hypotheses(&*self.db, &self.hypotheses);
        self
    }
}

/// Convenience function to create an iterator trait object.
/// Allows you to easily return multiple iterators from an `-> impl Iterator` function.
pub trait Boxed: Iterator {
    fn boxed(self) -> Box<dyn Iterator<Item = Self::Item>>;
}

impl<I: Iterator + 'static> Boxed for I {
    fn boxed(self) -> Box<dyn Iterator<Item = Self::Item>> {
        Box::new(self)
    }
}
