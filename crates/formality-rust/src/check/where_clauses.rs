use crate::grammar::{Fallible, Wcs};
use crate::prove::prove::{Env, Program};
use crate::{grammar::WhereClause, prove::ToWcs};
use fn_error_context::context;
use formality_core::judgment::ProofTree;

#[context("prove_where_clauses_well_formed({where_clauses:?})")]
pub(crate) fn prove_where_clauses_well_formed(
    program: &Program,
    env: &Env,
    assumptions: impl ToWcs,
    where_clauses: &[WhereClause],
) -> Fallible<ProofTree> {
    let wcs: Wcs = where_clauses
        .into_iter()
        .flat_map(|wc| wc.well_formed().into_iter())
        .collect();
    super::prove_goal(program, env, assumptions, wcs)
}
