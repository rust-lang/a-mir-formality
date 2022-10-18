use formality_types::grammar::ProgramClause;

use crate::grammar::{Crate, Program};

impl Crate {
    pub fn to_clauses(&self, program: &Program) -> Vec<ProgramClause> {
        let Crate { id: _, items } = self;
        items
            .iter()
            .flat_map(|item| item.to_clauses(program))
            .collect()
    }
}
