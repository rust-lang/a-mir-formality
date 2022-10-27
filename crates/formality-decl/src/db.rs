use formality_types::grammar::AtomicPredicate;

use crate::grammar::Program;

impl formality_types::db::Database for Program {
    fn program_clauses(
        &self,
        _predicate: &AtomicPredicate,
    ) -> Vec<formality_types::grammar::ProgramClause> {
        self.to_clauses()
    }

    fn invariants_for_predicate(
        &self,
        _predicate: &AtomicPredicate,
    ) -> Vec<formality_types::grammar::Invariant> {
        self.to_invariants()
    }
}
