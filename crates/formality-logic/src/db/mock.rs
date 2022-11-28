use formality_types::{
    grammar::{AtomicPredicate, Hypothesis, Invariant, ProgramClause, APR},
    parse::term,
};

use super::Db;

#[derive(Debug)]
pub struct MockDatabase {
    program_clauses: Vec<Hypothesis>,
    invariants: Vec<Invariant>,
}

impl MockDatabase {
    /// Creates a mock database that can be populated and then converted to a `Db` with `into_db`
    pub fn new() -> Self {
        MockDatabase {
            program_clauses: vec![],
            invariants: vec![],
        }
    }

    #[track_caller]
    pub fn with_program_clause(mut self, text: &str) -> Self {
        self.program_clauses.push(term(text));
        self
    }

    #[track_caller]
    pub fn with_invariant(mut self, text: &str) -> Self {
        self.invariants.push(term(text));
        self
    }

    pub fn into_db(self) -> Db {
        Db::new(self)
    }

    pub fn empty() -> Db {
        Self::new().into_db()
    }
}

impl crate::db::Database for MockDatabase {
    fn program_clauses(&self, _predicate: &AtomicPredicate) -> Vec<ProgramClause> {
        self.program_clauses.clone()
    }

    fn invariants_for_predicate(&self, _predicate: &AtomicPredicate) -> Vec<Invariant> {
        self.invariants.clone()
    }

    fn force_ambiguous(&self, _env: &crate::Env, _apr: &APR) -> bool {
        false
    }
}
