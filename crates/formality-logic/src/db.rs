use std::{fmt::Debug, sync::Arc};

use formality_types::grammar::{
    AtomicPredicate, AtomicRelation, Goal, Hypothesis, Invariant, ProgramClause,
};

pub trait Database: Debug {
    fn program_clauses(&self, goal: &Goal) -> Vec<ProgramClause>;
    fn invariants_for_predicate(&self, predicate: &AtomicPredicate) -> Vec<Invariant>;
    fn elaborate_relation(&self, r: &AtomicRelation) -> Vec<Hypothesis>;
}

/// A handle to the database. Only equal to itself.
#[derive(Clone)]
pub struct Db {
    db: Arc<dyn Database + Send>,
}

impl Db {
    pub fn new(db: impl Database + Send + 'static) -> Self {
        Self { db: Arc::new(db) }
    }
}

impl Debug for Db {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Db").finish()
    }
}

impl PartialEq for Db {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.db, &other.db)
    }
}

impl Eq for Db {}

impl PartialOrd for Db {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Arc::as_ptr(&self.db).partial_cmp(&Arc::as_ptr(&other.db))
    }
}

impl Ord for Db {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Arc::as_ptr(&self.db).cmp(&Arc::as_ptr(&other.db))
    }
}

impl std::hash::Hash for Db {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.db).hash(state)
    }
}

impl Database for Db {
    fn invariants_for_predicate(&self, predicate: &AtomicPredicate) -> Vec<Invariant> {
        self.db.invariants_for_predicate(predicate)
    }

    fn elaborate_relation(&self, r: &AtomicRelation) -> Vec<Hypothesis> {
        self.db.elaborate_relation(r)
    }

    fn program_clauses(&self, goal: &Goal) -> Vec<ProgramClause> {
        self.db.program_clauses(goal)
    }
}
