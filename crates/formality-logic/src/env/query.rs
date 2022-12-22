use std::collections::BTreeSet;

use formality_macros::term;
use formality_types::grammar::{
    AtomicRelation, Binder, ElaboratedHypotheses, Goal, InferenceVar, Universe,
};

use super::Env;

mod extract_query_result;
mod querify;
mod test;

/// A `Query` is a canonical description of a goal to be proven under a certain set of
/// assumptions, along with a minimal environment in which to prove it.
///
/// The environment contains only the (unbound) inference variables
/// that appear in the assumptions or goal; the numbering of those variables is consistent
/// such that any two queries with same assumptions/goals will have the same numbering
/// (e.g., left-to-right in order of appearance).
///
/// The environment also contains only the universes for placeholders that appear in the
/// assumptions/goals, and those universes are compresssed so they don't contain any
/// gaps.
#[term]
pub struct Query {
    pub env: Env,
    pub assumptions: ElaboratedHypotheses,
    pub goal: Goal,
}

impl Query {
    /// Returns a list of the input variables of the query.
    pub fn query_variables(&self) -> Vec<InferenceVar> {
        self.env.inference_variables()
    }

    /// Given the final enviroment that resulted from attempting to prove a query,
    /// returns the *query result*, which encodes the relations that have to be
    /// added to the query caller's environment to recreate this final environment.
    pub fn extract_result(&self, final_env: &Env) -> QueryResult {
        extract_query_result::extract_query_result(self, final_env)
    }
}

#[term]
pub struct UniverseMap {
    pub universes: Vec<(Universe, Universe)>,
}

#[term]
pub struct QueryResult {
    /// "Forall" variables in the query result. These are to be
    /// instantiated as existentials.
    pub binder: Binder<QueryResultBoundData>,
}

#[term]
pub struct QueryResultBoundData {
    /// Non-equality relations between inference variables in the initial environment
    /// or fresh variables. For example, `a: b`.
    pub relations: Vec<AtomicRelation>,
}

impl QueryResult {
    /// An "identity" result means that it adds no relations into the caller's environment.
    ///
    /// When the result is successful, this means it is true for any value of the query's inference variables.
    ///
    /// When the result is ambiguous, this means that it adds no information to the caller's environment that would help to resolve
    /// the ambiguity.
    pub fn identity() -> Self {
        QueryResult {
            binder: Binder::new(&[], QueryResultBoundData { relations: vec![] }),
        }
    }
}

pub use querify::querify;

/// Helper: Remove duplicates from `vec`, preserving the ordering.
fn dedup<T: Clone + Ord>(vec: &mut Vec<T>) {
    let mut set: BTreeSet<T> = BTreeSet::default();
    vec.retain(|e| set.insert(e.clone()));
}
