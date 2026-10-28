use crate::grammar::{Const, Fallible, Predicates, Variable};
use crate::prove::{enter_universally, Env, Program};
use crate::{grammar::WhereClause, prove::ToPredicates};
use fn_error_context::context;
use formality_core::judgment::ProofTree;

#[context("prove_where_clauses_well_formed({where_clauses:?})")]
pub(crate) fn prove_where_clauses_well_formed(
    program: &Program,
    env: &Env,
    assumptions: impl ToPredicates,
    where_clauses: &[WhereClause],
) -> Fallible<ProofTree> {
    for where_clause in where_clauses {
        check_const_bound_parameter(env, where_clause)?;
    }

    let wcs: Predicates = where_clauses
        .into_iter()
        .flat_map(|wc| wc.well_formed().into_iter())
        .collect();
    super::prove_goal(program, env, assumptions, wcs)
}

fn check_const_bound_parameter(env: &Env, where_clause: &WhereClause) -> Fallible<()> {
    match where_clause {
        WhereClause::TypeOfConst(constant, _) => {
            if !matches!(constant, Const::Variable(Variable::UniversalVar(_))) {
                anyhow::bail!(
                    "type_of_const requires a universal const parameter, found {constant:?}"
                );
            }
        }
        WhereClause::ForAll(binder) => {
            let (_, (env, inner)) = enter_universally(env, binder);
            check_const_bound_parameter(&env, &inner)?;
        }
        _ => {}
    }

    Ok(())
}
