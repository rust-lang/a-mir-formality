use formality_types::grammar::{Hypothesis, KindedVarIndex, Predicate, ProgramClause, TraitRef};

use crate::grammar::{ImplItem, Program, TraitImpl, TraitImplBoundData};

impl TraitImpl {
    pub fn to_clauses(&self, program: &Program) -> Vec<ProgramClause> {
        let (
            impl_kinded_var_ids,
            TraitImplBoundData {
                trait_ref,
                where_clauses,
                impl_items,
            },
        ) = self.binder.open();

        // The main rule for the impl:
        //
        // ∀. has_impl(TraitRef) :- where_clauses
        let has_impl = Hypothesis::for_all(
            &impl_kinded_var_ids,
            Hypothesis::implies(&where_clauses, trait_ref.has_impl()),
        );

        std::iter::once(has_impl)
            .chain(impl_items.iter().flat_map(|ii| {
                ii.to_clauses(&impl_kinded_var_ids, &trait_ref, &where_clauses, program)
            }))
            .collect()
    }
}

impl ImplItem {
    pub fn to_clauses(
        &self,
        impl_kinded_var_ids: &[KindedVarIndex],
        trait_ref: &TraitRef,
        impl_where_clauses: &[Predicate],
        program: &Program,
    ) -> Vec<ProgramClause> {
        match self {
            ImplItem::Fn(v) => v.to_clauses(impl_kinded_var_ids, program),
            ImplItem::AssociatedTyValue(v) => {
                v.to_clauses(impl_kinded_var_ids, trait_ref, impl_where_clauses, program)
            }
        }
    }
}
