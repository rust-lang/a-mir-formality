use formality_types::grammar::{BoundVar, ProgramClause};

use crate::grammar::Fn;

impl Fn {
    pub fn to_clauses(
        &self,
        _inherited_bound_vars: &[BoundVar],
        _program: &crate::grammar::Program,
    ) -> Vec<ProgramClause> {
        vec![]
    }
}
