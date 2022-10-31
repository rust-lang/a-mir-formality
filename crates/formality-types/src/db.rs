use std::{fmt::Debug, sync::Arc};

use crate::grammar::{AtomicPredicate, Invariant, ProgramClause};

pub mod mock;

pub trait Database: Debug {
    fn program_clauses(&self, predicate: &AtomicPredicate) -> Vec<ProgramClause>;
    fn invariants_for_predicate(&self, predicate: &AtomicPredicate) -> Vec<Invariant>;
}

/// A handle to the database. Only equal to itself.
#[derive(Clone)]
pub struct Db {
    db: Arc<dyn Database + Send>,
    solver_config: SolverConfiguration,
}

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Debug, Hash)]
pub enum SolverConfiguration {
    Cosld,
}

impl Db {
    pub fn new(db: impl Database + Send + 'static) -> Self {
        Self {
            db: Arc::new(db),
            solver_config: SolverConfiguration::Cosld,
        }
    }

    pub fn solver_config(&self) -> SolverConfiguration {
        self.solver_config
    }

    fn fields(&self) -> (*const (dyn Database + Send), &SolverConfiguration) {
        let Db { db, solver_config } = self;
        (Arc::as_ptr(db), solver_config)
    }
}

impl Debug for Db {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Db {
            db: _,
            solver_config,
        } = self;
        f.debug_struct("Db")
            .field("solver_config", solver_config)
            .finish()
    }
}

impl PartialEq for Db {
    fn eq(&self, other: &Self) -> bool {
        self.fields().eq(&other.fields())
    }
}

impl Eq for Db {}

impl PartialOrd for Db {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.fields().partial_cmp(&other.fields())
    }
}

impl Ord for Db {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.fields().cmp(&other.fields())
    }
}

impl std::hash::Hash for Db {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.fields().hash(state)
    }
}

impl Database for Db {
    fn invariants_for_predicate(&self, predicate: &AtomicPredicate) -> Vec<Invariant> {
        self.db.invariants_for_predicate(predicate)
    }

    fn program_clauses(&self, predicate: &AtomicPredicate) -> Vec<ProgramClause> {
        self.db.program_clauses(predicate)
    }
}
