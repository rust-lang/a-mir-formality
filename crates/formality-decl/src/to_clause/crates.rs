use formality_types::grammar::{Invariant, ProgramClause};

use crate::grammar::{Crate, Program};

impl Crate {
    pub(super) fn to_clauses(&self, program: &Program) -> Vec<ProgramClause> {
        let Crate { id: _, items } = self;
        items
            .iter()
            .flat_map(|item| item.to_clauses(program))
            .collect()
    }

    pub(super) fn to_invariants(&self, program: &Program) -> Vec<Invariant> {
        let Crate { id: _, items } = self;
        items
            .iter()
            .flat_map(|item| item.to_invariants(program))
            .collect()
    }
}
