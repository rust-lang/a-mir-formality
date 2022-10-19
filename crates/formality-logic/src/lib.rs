use formality_types::grammar::{AtomicPredicate, AtomicRelation, Hypothesis, Invariant};

pub trait Db {
    fn invariants_for_predicate(&self, predicate: &AtomicPredicate) -> Vec<Invariant>;
    fn elaborate_relation(&self, r: &AtomicRelation) -> Vec<Hypothesis>;
}

mod cosld;
mod elaborate_hypotheses;
