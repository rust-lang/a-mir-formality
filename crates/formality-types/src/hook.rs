use crate::env::Env;
use crate::grammar::{
    AtomicPredicate, AtomicRelation, Fallible, Goals, Hypotheses, Hypothesis, Invariant,
};

pub trait Hook {
    fn clauses(
        &self,
        env: &Env,
        hypotheses: &Hypotheses,
        predicate: &AtomicPredicate,
    ) -> Vec<Hypothesis>;

    fn invariants(
        &self,
        env: &Env,
        hypotheses: &Hypotheses,
        predicate: &AtomicPredicate,
    ) -> Vec<Invariant>;

    fn relate(&self, env: &Env, relation: &AtomicRelation) -> Fallible<(Env, Goals)>;
}
