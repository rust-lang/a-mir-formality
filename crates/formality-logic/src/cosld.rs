use derive_new::new;
use std::{collections::VecDeque, sync::Arc};

use formality_types::{
    env::Env,
    grammar::{AtomicPredicate, Goal, GoalData, Hypothesis, HypothesisData},
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

#[derive(new, Clone)]
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
    fn with_hypotheses(mut self, hypotheses: &[Hypothesis]) -> Self {
        self.hypotheses.extend(hypotheses.iter().cloned());
        self.hypotheses = elaborate_hypotheses::elaborate_hypotheses(&*self.db, &self.hypotheses);
        self
    }

    fn with_env(self, env: Env) -> Self {
        Self { env, ..self }
    }

    /// Prove the goal, yielding an iterator of possible solutions.
    fn prove_goal(mut self, goal: &Goal) -> impl Iterator<Item = CosldResult> {
        match goal.data() {
            GoalData::AtomicPredicate(predicate) => {
                self.prove_predicate_from_hypothesis(predicate).boxed()
            }
            GoalData::AtomicRelation(_) => todo!(),
            GoalData::ForAll(binder) => {
                let subgoal = self.env.instantiate_universally(binder);
                self.prove_goal(&subgoal).boxed()
            }
            GoalData::Exists(binder) => {
                let subgoal = self.env.instantiate_existentially(binder);
                self.prove_goal(&subgoal).boxed()
            }
            GoalData::Implies(conditions, subgoal) => {
                self.with_hypotheses(conditions).prove_goal(subgoal).boxed()
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

    fn prove_all(self, subgoals: &[Goal]) -> impl Iterator<Item = CosldResult> {
        self.prove_all_deferred(subgoals.iter().cloned().collect(), vec![])
    }

    /// Prove a list of goals in any order, returning an iterator over solutions or ambiguous results.
    /// (An empty iterator indicates the goal is not provable.)
    ///
    /// # Parameters
    ///
    /// * subgoals is the list of goals to try, in some order.
    /// * deferred is a list of goals that have previously come up ambiguous.
    fn prove_all_deferred(
        self,
        mut subgoals: VecDeque<Goal>,
        deferred: Vec<Goal>,
    ) -> impl Iterator<Item = CosldResult> {
        if let Some(subgoal) = subgoals.pop_front() {
            let this = self.clone();
            self.prove_goal(&subgoal)
                .flat_map(move |result| {
                    let mut subgoals = subgoals.clone();
                    let mut deferred = deferred.clone();
                    match result {
                        CosldResult::Yes(env) => {
                            // If we proved something, that may cause deferred goals to be unambig, so move
                            // them over to the subgoals list (at the back, which doesn't really matter, but
                            // makes sense).
                            subgoals.extend(deferred.drain(..));
                            this.clone()
                                .with_env(env)
                                .prove_all_deferred(subgoals.clone(), deferred.clone())
                        }
                        CosldResult::Maybe => {
                            // Ambiguous result: push it for later.
                            deferred.push(subgoal.clone());
                            this.clone().prove_all_deferred(subgoals, deferred)
                        }
                    }
                })
                .boxed()
        } else if deferred.is_empty() {
            std::iter::once(CosldResult::Yes(self.env)).boxed()
        } else {
            std::iter::once(CosldResult::Maybe).boxed()
        }
    }

    fn prove_predicate_from_hypothesis(
        &self,
        predicate: &AtomicPredicate,
    ) -> impl Iterator<Item = CosldResult> {
        self.hypotheses
            .clone()
            .into_iter()
            .filter({
                let predicate = predicate.clone();
                move |h| h.could_match(&predicate)
            })
            .flat_map({
                let this = self.clone();
                let predicate = predicate.clone();
                move |h| this.clone().prove_match_hypothesis(&h, &predicate)
            })
    }

    fn prove_match_hypothesis(
        mut self,
        hypothesis: &Hypothesis,
        predicate: &AtomicPredicate,
    ) -> impl Iterator<Item = CosldResult> {
        match hypothesis.data() {
            HypothesisData::AtomicPredicate(h) => {
                let (skeleton1, parameters1) = h.debone();
                let (skeleton2, parameters2) = predicate.debone();
                if skeleton1 != skeleton2 {
                    std::iter::empty().boxed()
                } else {
                    assert_eq!(parameters1.len(), parameters2.len());
                    let subgoals: Vec<Goal> = parameters1
                        .into_iter()
                        .zip(parameters2)
                        .map(|(p1, p2)| Goal::eq(p1, p2))
                        .collect();
                    self.prove_all(&subgoals).boxed()
                }
            }
            HypothesisData::AtomicRelation(_h) => todo!(),
            HypothesisData::ForAll(b) => {
                let h = self.env.instantiate_existentially(b);
                self.prove_match_hypothesis(&h, predicate).boxed()
            }
            HypothesisData::Implies(conditions, consequence) => {
                let this = self.clone();
                let conditions = conditions.clone();
                self.prove_match_hypothesis(consequence, predicate)
                    .flat_map(move |result| match result {
                        CosldResult::Maybe => std::iter::once(CosldResult::Maybe).boxed(),
                        CosldResult::Yes(env) => {
                            this.clone().with_env(env).prove_all(&conditions).boxed()
                        }
                    })
                    .boxed()
            }
            HypothesisData::CoherenceMode => std::iter::empty().boxed(),
        }
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
