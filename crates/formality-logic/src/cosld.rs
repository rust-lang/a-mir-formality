use formality_macros::term;

use formality_types::{
    collections::Set,
    env::Env,
    grammar::{AtomicPredicate, Goal, GoalData, Hypothesis, HypothesisData},
    set,
};

use crate::{db::Database, elaborate_hypotheses::elaborate_hypotheses, Db};

/// Prove a "top-level" goal is true in the given environment
/// using the cosld solver. cosld is a basic [SLD] solving algorithm,
/// enriched to handle [FOHH] predicates as well as to
/// support a simple form of coinduction.
///
/// [SLD]: https://en.wikipedia.org/wiki/SLD_resolution
/// [FOHH]: https://en.wikipedia.org/wiki/Harrop_formula
pub fn prove(db: &Db, env: &Env, assumptions: &Vec<Hypothesis>, goal: &Goal) -> Set<CosldResult> {
    let assumptions = &elaborate_hypotheses(db, assumptions);
    prove_goal(db, env, assumptions, goal)
}

fn prove_goal(db: &Db, env: &Env, assumptions: &Set<Hypothesis>, goal: &Goal) -> Set<CosldResult> {
    match goal.data() {
        GoalData::AtomicPredicate(predicate) => {
            let clauses = db.program_clauses(predicate);
            clauses
                .iter()
                .chain(assumptions)
                .filter(|h| h.could_match(predicate))
                .flat_map(|h| backchain(db, env, assumptions, h, predicate))
                .collect()
        }
        GoalData::AtomicRelation(_) => todo!(),
        GoalData::ForAll(binder) => {
            let mut env = env.clone();
            let subgoal = env.instantiate_universally(binder);
            prove_goal(db, &env, assumptions, &subgoal)
        }
        GoalData::Exists(binder) => {
            let mut env = env.clone();
            let subgoal = env.instantiate_existentially(binder);
            prove_goal(db, &env, assumptions, &subgoal)
        }
        GoalData::Implies(conditions, subgoal) => {
            let assumptions = elaborate_hypotheses(db, assumptions.iter().chain(conditions));
            prove_goal(db, env, &assumptions, subgoal)
        }
        GoalData::Any(subgoals) => subgoals
            .iter()
            .flat_map(move |subgoal| prove_goal(db, env, assumptions, subgoal))
            .collect(),
        GoalData::All(subgoals) => prove_all(db, &env, assumptions, subgoals, &[]),
        GoalData::CoherenceMode(subgoal) => {
            let env = env.clone();
            let env = env.in_coherence_mode();
            prove_goal(db, &env, assumptions, subgoal)
        }
        GoalData::Ambiguous => set![CosldResult::Maybe],
    }
}

fn prove_all(
    db: &Db,
    env: &Env,
    assumptions: &Set<Hypothesis>,
    subgoals: &[Goal],
    deferred: &[Goal],
) -> Set<CosldResult> {
    if let Some((subgoal, remainder)) = subgoals.split_first() {
        prove_goal(db, env, assumptions, subgoal)
            .into_iter()
            .flat_map(|r| match r {
                CosldResult::Yes(env1) => {
                    let mut subgoals = remainder.to_owned();
                    subgoals.extend(deferred.iter().cloned());
                    prove_all(db, &env1, assumptions, &subgoals, &[])
                }
                CosldResult::Maybe => {
                    let mut deferred = deferred.to_owned();
                    deferred.push(subgoal.clone());
                    prove_all(db, env, assumptions, remainder, &deferred)
                }
            })
            .collect()
    } else if !deferred.is_empty() {
        set![CosldResult::Maybe]
    } else {
        set![]
    }
}

/// "Backchaining" is the process of applying a program clause or assumption
/// (both a kind of [`Hypothesis`]) to prove `predicate` is true. The idea is to
/// match `predicate` against `clause` andd prove any additional conditions.
///
/// For example, if `clause` is `implies([P, Q], R)`,
/// then we would match `predicate` against `R` and -- if that succeeds --
/// try to prove `P` and `Q`.
fn backchain(
    db: &Db,
    env: &Env,
    assumptions: &Set<Hypothesis>,
    clause: &Hypothesis,
    predicate: &AtomicPredicate,
) -> Set<CosldResult> {
    match clause.data() {
        HypothesisData::AtomicPredicate(h) => {
            let (skeleton1, parameters1) = h.debone();
            let (skeleton2, parameters2) = predicate.debone();
            if skeleton1 != skeleton2 {
                set![]
            } else {
                assert_eq!(parameters1.len(), parameters2.len());
                let subgoals: Vec<Goal> = parameters1
                    .into_iter()
                    .zip(parameters2)
                    .map(|(p1, p2)| Goal::eq(p1, p2))
                    .collect();
                prove_all(db, env, assumptions, &subgoals, &[])
            }
        }
        HypothesisData::AtomicRelation(_h) => todo!(),
        HypothesisData::ForAll(b) => {
            let mut env = env.clone();
            let h = env.instantiate_existentially(b);
            backchain(db, &env, assumptions, &h, predicate)
        }
        HypothesisData::Implies(conditions, consequence) => {
            backchain(db, env, assumptions, consequence, predicate)
                .and_then(|env| prove_all(db, env, assumptions, &conditions, &[]))
        }
        HypothesisData::CoherenceMode => set![],
    }
}

trait AndThen {
    fn and_then(self, op: impl Fn(&Env) -> Set<CosldResult>) -> Set<CosldResult>;
}

impl AndThen for Set<CosldResult> {
    fn and_then(self, op: impl Fn(&Env) -> Set<CosldResult>) -> Set<CosldResult> {
        self.into_iter()
            .flat_map(|r| match r {
                CosldResult::Maybe => set![CosldResult::Maybe],
                CosldResult::Yes(env) => op(&env),
            })
            .collect()
    }
}

#[term]
pub enum CosldResult {
    Yes(Env),
    Maybe,
}
