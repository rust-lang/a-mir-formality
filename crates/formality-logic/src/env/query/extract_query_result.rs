use std::collections::{BTreeSet, VecDeque};

use formality_types::{
    cast::{Downcasted, Upcasted},
    collections::Set,
    fold::Fold,
    grammar::{
        fresh_bound_var, AtomicRelation, Binder, BoundVar, InferenceVar, VarSubstitution, Variable,
    },
};

use crate::Env;

use super::{Query, QueryResult, QueryResultBoundData};

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

fn relations_from(final_env: &Env, query_variables: &[InferenceVar]) -> Vec<AtomicRelation> {
    let mut all_variables: Set<Variable> = BTreeSet::new();
    let mut queue: VecDeque<Variable> = query_variables.iter().upcasted().collect();
    let mut relations = vec![];

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
        queue.extend(new_relations.free_variables());
        relations.extend(new_relations);
    }

    relations
}

fn close_over(
    final_env: &Env,
    query_variables: &[InferenceVar],
    relations: &Vec<AtomicRelation>,
) -> QueryResult {
    let all_variables = relations.free_variables();

    let new_inference_variables: Vec<InferenceVar> = all_variables
        .iter()
        .downcasted::<InferenceVar>()
        .filter(|v| !query_variables.contains(v))
        .collect();

    let fresh_bound_variables: Vec<BoundVar> = new_inference_variables
        .iter()
        .map(|&v| fresh_bound_var(final_env.data(v).kind))
        .collect();

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
