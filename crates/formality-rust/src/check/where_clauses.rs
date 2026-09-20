use crate::grammar::{Fallible, Goals};
use crate::prove::{Env, Program};
use crate::{grammar::WhereClause, prove::ToGoals};
use fn_error_context::context;
use formality_core::judgment::ProofTree;

#[context("prove_where_clauses_well_formed({where_clauses:?})")]
pub(crate) fn prove_where_clauses_well_formed(
    program: &Program,
    env: &Env,
    assumptions: impl ToGoals,
    where_clauses: &[WhereClause],
) -> Fallible<ProofTree> {
    let goals: Goals = where_clauses
        .into_iter()
        .flat_map(|wc| wc.well_formed().into_iter())
        .collect();
    super::prove_goal(program, env, assumptions, goals)
}
