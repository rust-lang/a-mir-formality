use crate::env::Env;
use crate::grammar::{AtomicPredicate, AtomicRelation, Fallible, Goals, Hypotheses, Invariants};

pub trait Hook {
    fn clauses(
        &self,
        env: &Env,
        hypotheses: &Hypotheses,
        predicate: &AtomicPredicate,
    ) -> Hypotheses;

    fn invariants(
        &self,
        env: &Env,
        hypotheses: &Hypotheses,
        predicate: &AtomicPredicate,
    ) -> Invariants;

    fn relate(&self, env: &Env, relation: &AtomicRelation) -> Fallible<(Env, Goals)>;
}
