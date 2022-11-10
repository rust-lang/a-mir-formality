use formality_types::grammar::{AtomicPredicate, APR};

use crate::grammar::Program;

impl formality_logic::Database for Program {
    fn program_clauses(
        &self,
        _predicate: &AtomicPredicate,
    ) -> Vec<formality_types::grammar::ProgramClause> {
        self.to_clauses()
    }

    fn invariants_for_apr(&self, _predicate: &APR) -> Vec<formality_types::grammar::Invariant> {
        self.to_invariants()
    }
}
