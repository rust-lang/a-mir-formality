use formality_infer::Env;
use formality_macros::term;

use formality_types::{
    collections::Set,
    db::{Database, Db},
    grammar::{Coinductive, ElaboratedHypotheses, Goal, GoalData, Hypothesis, HypothesisData, APR},
    set,
};

use crate::elaborate_hypotheses::elaborate_hypotheses;

mod test;

/// Prove a "top-level" goal is true in the given environment
/// using the cosld solver. cosld is a basic [SLD] solving algorithm,
/// enriched to handle [FOHH] predicates as well as to
/// support a simple form of coinduction.
///
/// [SLD]: https://en.wikipedia.org/wiki/SLD_resolution
/// [FOHH]: https://en.wikipedia.org/wiki/Harrop_formula
pub fn prove(db: &Db, env: &Env, assumptions: &[Hypothesis], goal: &Goal) -> Set<CosldResult> {
    let assumptions = &elaborate_hypotheses(db, assumptions);
    prove_goal(db, env, Stack::Empty, assumptions, goal)
}

#[derive(Copy, Clone, Debug)]
enum Stack<'s> {
    Empty,
    Link(&'s APR, &'s Stack<'s>),
}

#[tracing::instrument(level = "debug", ret)]
fn prove_goal(
    db: &Db,
    env: &Env,
    stack: Stack<'_>,
    assumptions: &ElaboratedHypotheses,
    goal: &Goal,
) -> Set<CosldResult> {
    match goal.data() {
        GoalData::Atomic(apr) => match stack.search(env, apr) {
            StackSearch::CoinductiveCycle => set![CosldResult::Yes(env.clone())],
            StackSearch::InductiveCycle => set![],
            StackSearch::NotFound => match apr {
                APR::AtomicPredicate(predicate) => {
                    let clauses = db.program_clauses(predicate);
                    clauses
                        .iter()
                        .chain(assumptions)
                        .filter(|h| h.could_match(predicate))
                        .flat_map(|h| backchain(db, env, stack, assumptions, h, apr))
                        .collect()
                }

                APR::AtomicRelation(r) => match env.apply_relation(db, assumptions, r) {
                    Ok((env, goals)) => {
                        prove_all(db, &env, Stack::Link(apr, &stack), assumptions, &goals, &[])
                    }
                    Err(_) => set![],
                },
            },
        },
        GoalData::ForAll(binder) => {
            let mut env = env.clone();
            let subgoal = env.instantiate_universally(binder);
            prove_goal(db, &env, stack, assumptions, &subgoal)
        }
        GoalData::Exists(binder) => {
            let mut env = env.clone();
            let subgoal = env.instantiate_existentially(binder);
            prove_goal(db, &env, stack, assumptions, &subgoal)
        }
        GoalData::Implies(conditions, subgoal) => {
            let assumptions = elaborate_hypotheses(db, assumptions.iter().chain(conditions));
            prove_goal(db, env, stack, &assumptions, subgoal)
        }
        GoalData::Any(subgoals) => subgoals
            .iter()
            .flat_map(move |subgoal| prove_goal(db, env, stack, assumptions, subgoal))
            .collect(),
        GoalData::All(subgoals) => prove_all(db, &env, stack, assumptions, subgoals, &[]),
        GoalData::CoherenceMode(subgoal) => {
            let env = env.clone();
            let env = env.in_coherence_mode();
            prove_goal(db, &env, stack, assumptions, subgoal)
        }
        GoalData::Ambiguous => set![CosldResult::Maybe],
    }
}

/// Prove a sequence of subgoals to be true, yielding the final results.
/// The order of the subgoals is immaterial.
///
/// To manage connections between subgoals (e.g., cases where proving one subgoal
/// constraints inference variables found in another subgoal), we do a limited
/// amount of reordering:
///
/// * If a subgoal is found to be ambiguous, then it is pushed into the "deferred" list.
/// * When another subgoal succeeds, the deferred list is added back into the list of goals to be proven.
/// * If we wind up with deferred goals and no more subgoals, the final result is ambiguous.
#[tracing::instrument(level = "debug", ret)]
fn prove_all(
    db: &Db,
    env: &Env,
    stack: Stack<'_>,
    assumptions: &ElaboratedHypotheses,
    subgoals: &[Goal],
    deferred: &[Goal],
) -> Set<CosldResult> {
    if let Some((subgoal, remainder)) = subgoals.split_first() {
        prove_goal(db, env, stack, assumptions, subgoal)
            .into_iter()
            .flat_map(|r| match r {
                CosldResult::Yes(env) if deferred.is_empty() => {
                    // Pointless microoptimization.
                    prove_all(db, &env, stack, assumptions, remainder, &[])
                }
                CosldResult::Yes(env) => {
                    let mut subgoals = remainder.to_owned();
                    subgoals.extend(deferred.iter().cloned());
                    prove_all(db, &env, stack, assumptions, &subgoals, &[])
                }
                CosldResult::Maybe => {
                    let mut deferred = deferred.to_owned();
                    deferred.push(subgoal.clone());
                    prove_all(db, env, stack, assumptions, remainder, &deferred)
                }
            })
            .collect()
    } else if !deferred.is_empty() {
        set![CosldResult::Maybe]
    } else {
        set![CosldResult::Yes(env.clone())]
    }
}

/// "Backchaining" is the process of applying a program clause or assumption
/// (both a kind of [`Hypothesis`]) to prove `predicate` is true. The idea is to
/// match `predicate` against `clause` andd prove any additional conditions.
///
/// For example, if `clause` is `implies([P, Q], R)`,
/// then we would match `predicate` against `R` and -- if that succeeds --
/// try to prove `P` and `Q`.
#[tracing::instrument(level = "debug", ret)]
fn backchain(
    db: &Db,
    env: &Env,
    stack: Stack<'_>,
    assumptions: &ElaboratedHypotheses,
    clause: &Hypothesis,
    apr: &APR,
) -> Set<CosldResult> {
    match clause.data() {
        HypothesisData::Atomic(h) => {
            let (skeleton1, parameters1) = h.debone();
            let (skeleton2, parameters2) = apr.debone();
            if skeleton1 != skeleton2 {
                set![]
            } else {
                assert_eq!(parameters1.len(), parameters2.len());
                let subgoals: Vec<Goal> = parameters1
                    .into_iter()
                    .zip(parameters2)
                    .map(|(p1, p2)| Goal::eq(p1, p2))
                    .collect();
                prove_all(db, env, stack, assumptions, &subgoals, &[])
            }
        }
        HypothesisData::ForAll(b) => {
            let mut env = env.clone();
            let h = env.instantiate_existentially(b);
            backchain(db, &env, stack, assumptions, &h, apr)
        }
        HypothesisData::Implies(conditions, consequence) => {
            backchain(db, env, stack, assumptions, consequence, apr).and_then(|env| {
                prove_all(
                    db,
                    env,
                    Stack::Link(apr, &stack),
                    assumptions,
                    &conditions,
                    &[],
                )
            })
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum StackSearch {
    CoinductiveCycle,
    InductiveCycle,
    NotFound,
}

impl Stack<'_> {
    /// Search the stack to see if it contains `predicate1`.
    /// If so, returns coinductive cycle if all stack entries (including `predicate1`) are coinductive,
    /// otherwise returns inductive cycle.
    #[tracing::instrument(level = "debug", ret)]
    fn search(self, env: &Env, predicate1: &APR) -> StackSearch {
        let predicate1 = env.refresh_inference_variables(predicate1);

        let mut p = self;
        let mut all_coinductive = Coinductive::Yes;
        loop {
            match p {
                Stack::Empty => return StackSearch::NotFound,
                Stack::Link(predicate2, link) => {
                    all_coinductive = all_coinductive & predicate2.is_coinductive();
                    if predicate1 == env.refresh_inference_variables(predicate2) {
                        return match all_coinductive {
                            Coinductive::Yes => StackSearch::CoinductiveCycle,
                            Coinductive::No => StackSearch::InductiveCycle,
                        };
                    } else {
                        p = *link;
                    }
                }
            }
        }
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
