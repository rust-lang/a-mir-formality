use formality_types::grammar::{AtomicRelation, Hypothesis};

pub trait Db {
    fn elaborate_relation(&self, r: &AtomicRelation) -> Vec<Hypothesis>;
}

mod cosld;
mod elaborate_hypotheses;
