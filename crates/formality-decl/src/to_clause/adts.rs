use crate::grammar::{Adt, Program};

impl Adt {
    pub fn to_clauses(&self, _program: &Program) -> Vec<formality_types::grammar::ProgramClause> {
        unimplemented!()
    }
}
