use formality_types::grammar::{ProgramClause, KindedVarIndex};

use crate::grammar::Fn;

impl Fn {
    pub fn to_clauses(
        &self,
        inherited_kinded_var_ids: &[KindedVarIndex],
        program: &crate::grammar::Program,
    ) -> Vec<ProgramClause> {
        unimplemented!()
    }
}
