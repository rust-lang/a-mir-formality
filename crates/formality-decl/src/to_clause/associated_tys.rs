use formality_types::grammar::{
    AliasTy, BoundVar, Hypothesis, Invariant, Predicate, ProgramClause, TraitRef,
};

use crate::grammar::{AssociatedTy, AssociatedTyValue, AssociatedTyValueBoundData, Program};

impl AssociatedTy {
    pub(super) fn to_clauses(
        &self,
        _trait_bound_vars: &[BoundVar],
        _program: &Program,
    ) -> Vec<ProgramClause> {
        unimplemented!()
    }

    pub(super) fn to_invariants(&self, _program: &Program) -> Vec<Invariant> {
        unimplemented!()
    }
}

impl AssociatedTyValue {
    pub(super) fn to_clauses(
        &self,
        impl_bound_vars: &[BoundVar],
        trait_ref: &TraitRef,
        impl_where_clauses: &[Predicate],
        _program: &Program,
    ) -> Vec<ProgramClause> {
        let (atv_kinded_var_ids, AssociatedTyValueBoundData { where_clauses, ty }) =
            self.binder.open();

        let alias_ty = AliasTy::associated_ty(
            trait_ref.trait_id.clone(),
            self.id.clone(),
            (&trait_ref.parameters, &atv_kinded_var_ids),
        );

        let g = Hypothesis::for_all(
            (impl_bound_vars, atv_kinded_var_ids),
            Hypothesis::implies(
                (impl_where_clauses, &where_clauses),
                alias_ty.normalizes_to(&ty),
            ),
        );

        vec![g]
    }

    pub(super) fn to_invariants(&self, _program: &Program) -> Vec<Invariant> {
        unimplemented!()
    }
}
