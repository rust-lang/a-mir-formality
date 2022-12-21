#![allow(dead_code)] // still figuring things out

use contracts::requires;
use cosld::CosldResult;
use formality_macros::term;
use formality_types::{
    derive_links,
    grammar::{Goal, Hypothesis},
    term::Term,
};

mod cosld;
mod db;
mod elaborate_hypotheses;
mod env;
mod recursive;

pub use crate::db::{mock::MockDatabase, Database, Db, SolverConfiguration};
pub use crate::env::Env;

#[term]
pub enum UniversalGoalResult {
    Yes,
    No,
    Maybe,
}

/// Proves a goal that references only univesal placeholders.
/// This is an important special case because it has no inference side effects.
#[tracing::instrument(ret)]
#[requires(goal.references_only_placeholder_variables())]
#[requires(assumptions.iter().all(|a| a.references_only_placeholder_variables()))]
pub fn prove_universal_goal(
    db: &Db,
    env: &Env,
    assumptions: &[Hypothesis],
    goal: &Goal,
) -> UniversalGoalResult {
    match db.solver_config() {
        SolverConfiguration::Cosld => {
            let results = cosld::prove(db, env, assumptions, goal);
            if results.iter().any(|r| matches!(r, CosldResult::Yes(_))) {
                UniversalGoalResult::Yes
            } else if results.iter().any(|r| matches!(r, CosldResult::Maybe)) {
                UniversalGoalResult::Maybe
            } else {
                UniversalGoalResult::No
            }
        }
    }
}

#[term]
pub enum GoalResult {
    /// Goal was proven in the given environment.
    Yes(Env),

    /// Goal may be provable, but we've not proven it yet.
    /// The returned environment may contain unifications that
    /// had to be true for the goal to be provable.
    Maybe(Env),

    /// Goal is not provable.
    No,
}

/// Prove a goal in the given environment with the given assumptions.
///
/// The `GoalResult` that is returned may contain a new environment:
/// that contains conditions and unifications that had to be true for
/// the goal to be true (this applies even if a "maybe" result is returned).
pub fn prove_goal(db: &Db, env: &Env, assumptions: &[Hypothesis], goal: &Goal) -> GoalResult {
    match db.solver_config() {
        SolverConfiguration::Cosld => {
            let results = cosld::prove(db, env, assumptions, goal);
            if results.len() == 0 {
                GoalResult::No
            } else if results.len() > 1 {
                // This is overly simplistic, it may be that there are multiple Yes results but
                // one is clearly better. I'm not going to deal with that right now.
                GoalResult::Maybe(env.clone())
            } else {
                match results.into_iter().next().unwrap() {
                    CosldResult::Yes(env) => GoalResult::Yes(env),
                    CosldResult::Maybe => GoalResult::Maybe(env.clone()),
                }
            }
        }
    }
}
