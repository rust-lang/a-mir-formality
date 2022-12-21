use std::collections::{BTreeSet, VecDeque};

use formality_types::{
    cast::{Downcasted, Upcasted},
    collections::Set,
    fold::Fold,
    grammar::{
        fresh_bound_var, AtomicRelation, Binder, BoundVar, InferenceVar, VarSubstitution, Variable,
    },
};

use crate::{env::query::dedup, Env};

use super::{Query, QueryResult, QueryResultBoundData};

/// Extracts the *query result* from the final env that resulted from solving the query.
/// This query result contains atomic relations that constrain the query env.
#[tracing::instrument(level = "debug", ret)]
pub fn extract_query_result(query: &Query, final_env: &Env) -> QueryResult {
    let query_variables = query.query_variables();

    // Find (transitive) relations on those query variables, along with any existential variables
    // that appear in their substitutions
    let relations = relations_from(final_env, &query_variables);

    // Assertion: Those relations should only reference things visible from the
    // universe of the query.
    assert!(final_env.term_universe(&relations) <= query.env.universe);

    close_over(final_env, &query_variables, &relations)
}

#[tracing::instrument(level = "debug", ret)]
fn relations_from(final_env: &Env, query_variables: &[InferenceVar]) -> Vec<AtomicRelation> {
    let mut all_variables: Set<Variable> = BTreeSet::new();
    let mut queue: VecDeque<Variable> = query_variables.iter().upcasted().collect();
    let mut relations = vec![];

    // Special case: for any query variable `?X` that has a mapped value `V`,
    // insert a `equal(?X, V)` relation where `?X` is *not* refreshed.
    // (If we refreshed `?X`, we would just get `equal(V, V)`, not very interesting.)
    // These equality relations are effectively the "substitution" that results
    // from this query, and they serve to constrain the caller.
    for &qv in query_variables {
        if let Some(p) = &final_env.data(qv).mapped_to {
            let p = final_env.refresh_inference_variables(p);
            queue.extend(p.free_variables());
            relations.push(AtomicRelation::eq(qv, p));
        }
    }

    // Fixed point: for each query variable `?X`, include all relations
    // relating `?X` to other variables. Then repeat this for any other
    // variables `?V` that appear in those relations.
    //
    // e.g., if you have a query variable `?X`, we may find that `?X: ?0`,
    // where `?0` is some fresh inference variable. We would then look at
    // any relations from `?0`, perhaps finding `?0: ?Y`, where `?Y` is another
    // inference variable. The final result would then include `?X: ?0`
    // and `?0: ?Y`.
    while let Some(v) = queue.pop_front() {
        if !all_variables.insert(v) {
            continue;
        }

        let iv = match v {
            Variable::InferenceVar(iv) => iv,
            Variable::PlaceholderVar(_) => continue,
            Variable::BoundVar(_) => panic!(),
        };

        let new_relations = final_env.inference_var_relations(iv);
        let new_relations = final_env.refresh_inference_variables(&new_relations);
        queue.extend(new_relations.free_variables());
        relations.extend(new_relations);
    }

    // Cleanup the output by removing trivial relations like `X == X`.
    // These arise as a result of inference variables mappings.
    // (E.g., the fully normalized form of `equals(?A, u32)` is `equals(u32, u32)`.)
    relations.retain(|r| !r.is_trivially_true());

    relations
}

#[tracing::instrument(level = "debug", ret)]
fn close_over(
    final_env: &Env,
    query_variables: &[InferenceVar],
    relations: &Vec<AtomicRelation>,
) -> QueryResult {
    let mut all_variables = relations.free_variables();
    dedup(&mut all_variables);
    tracing::debug!(?all_variables);

    let new_inference_variables: Vec<InferenceVar> = all_variables
        .iter()
        .downcasted::<InferenceVar>()
        .filter(|v| !query_variables.contains(v))
        .collect();
    tracing::debug!(?new_inference_variables);

    let fresh_bound_variables: Vec<BoundVar> = new_inference_variables
        .iter()
        .map(|&v| fresh_bound_var(final_env.data(v).kind))
        .collect();
    tracing::debug!(?fresh_bound_variables);

    let to_bound_variables: VarSubstitution = new_inference_variables
        .iter()
        .zip(&fresh_bound_variables)
        .upcasted()
        .collect();

    let bound_data = QueryResultBoundData {
        relations: to_bound_variables.apply(&relations),
    };

    QueryResult {
        binder: Binder::new(&fresh_bound_variables, bound_data),
    }
}
