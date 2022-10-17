use formality_core::all_into::AllInto;
use formality_types::grammar::{Binder, HypothesisData, ProgramClause};

use crate::grammar::{ImplItem, Program, TraitImpl, TraitImplBoundData};

use super::ToClause;

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
        let is_implemented = Hypothesis::for_all(
            &impl_kinded_var_ids,
            Hypothesis::implies(where_clauses, trait_ref.has_impl()),
        );

        std::iter::once(is_implemented)
            .chain(
                impl_items
                    .iter()
                    .flat_map(|ii| ii.to_clauses(&impl_kinded_var_ids, program)),
            )
            .collect()
    }
}

impl ImplItem {
    pub fn to_clauses(
        &self,
        impl_kinded_var_ids: &[KindedVarIndex],
        program: &Program,
    ) -> Vec<ProgramClause> {
        match self {
            ImplItem::Fn(v) => v.to_clauses(impl_kinded_var_ids, program),
            ImplItem::AssociatedTyValue(v) => v.to_clauses(impl_kinded_var_ids, program),
        }
    }
}
