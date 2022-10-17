use formality_types::grammar::ProgramClause;

use crate::grammar::{AssociatedTy, AssociatedTyValue, Fn};

impl AssociatedTy {
    pub fn to_clauses(
        &self,
        trait_kinded_var_ids: &[KindedVarIndex],
        program: &crate::grammar::Program,
    ) -> Vec<ProgramClause> {
        unimplemented!()
    }
}

impl AssociatedTyValue {
    pub fn to_clauses(
        &self,
        impl_kinded_var_ids: &[KindedVarIndex],
        program: &crate::grammar::Program,
    ) -> Vec<ProgramClause> {
        unimplemented!()
    }
}
