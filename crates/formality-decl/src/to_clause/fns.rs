use formality_types::grammar::{KindedVarIndex, ProgramClause};

use crate::grammar::Fn;

impl Fn {
    pub fn to_clauses(
        &self,
        _inherited_kinded_var_ids: &[KindedVarIndex],
        _program: &crate::grammar::Program,
    ) -> Vec<ProgramClause> {
        unimplemented!()
    }
}
