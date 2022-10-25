use formality_types::grammar::{
    AliasTy, Hypothesis, KindedVarIndex, Predicate, ProgramClause, TraitRef,
};

use crate::grammar::{AssociatedTy, AssociatedTyValue, AssociatedTyValueBoundData};

impl AssociatedTy {
    pub fn to_clauses(
        &self,
        _trait_kinded_var_ids: &[KindedVarIndex],
        _program: &crate::grammar::Program,
    ) -> Vec<ProgramClause> {
        unimplemented!()
    }
}

impl AssociatedTyValue {
    pub fn to_clauses(
        &self,
        impl_kinded_var_ids: &[KindedVarIndex],
        trait_ref: &TraitRef,
        impl_where_clauses: &[Predicate],
        _program: &crate::grammar::Program,
    ) -> Vec<ProgramClause> {
        let (atv_kinded_var_ids, AssociatedTyValueBoundData { where_clauses, ty }) =
            self.binder.open();

        let alias_ty = AliasTy::associated_ty(
            trait_ref.trait_id.clone(),
            self.id.clone(),
            (&trait_ref.parameters, &atv_kinded_var_ids),
        );

        let g = Hypothesis::for_all(
            (impl_kinded_var_ids, atv_kinded_var_ids),
            Hypothesis::implies(
                (impl_where_clauses, &where_clauses),
                alias_ty.normalizes_to(&ty),
            ),
        );

        vec![g]
    }
}
