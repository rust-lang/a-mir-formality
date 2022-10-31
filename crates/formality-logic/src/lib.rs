#![allow(dead_code)] // still figuring things out

use contracts::requires;
use cosld::CosldResult;
use formality_infer::Env;
use formality_macros::term;
use formality_types::{
    db::{Db, SolverConfiguration},
    derive_links,
    grammar::{Goal, Hypothesis},
    term::Term,
};

mod cosld;
mod elaborate_hypotheses;
mod recursive;

#[term]
pub enum UniversalGoalResult {
    Yes,
    No,
    Maybe,
}

#[requires(goal.references_only_placeholder_variables())]
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
