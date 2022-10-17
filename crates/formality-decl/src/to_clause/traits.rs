use formality_core::all_into::AllInto;
use formality_types::grammar::{Binder, HypothesisData, ProgramClause, TraitRef};

use crate::grammar::{TraitImpl, TraitImplBoundData};

use super::ToClause;

impl Trait {
    pub fn to_clauses(&self, program: &crate::grammar::Program) -> Vec<ProgramClause> {
        let (
            trait_kinded_var_ids,
            TraitBoundData {
                where_clauses,
                trait_items,
            },
        ) = self.binder.open();

        let trait_ref = TraitRef::new(self.id, &trait_kinded_var_ids);

        // The main rule for the trait:
        //
        // ∀. is_implemented(TraitRef) :- has_impl(TraitRef), where_clauses
        let is_implemented = Hypothesis::for_all(
            &trait_kinded_var_ids,
            Hypothesis::implies(
                (Some(trait_ref.has_impl()), where_clauses),
                trait_ref.is_implemented(),
            ),
        );

        std::iter::once(is_implemented)
            .chain(trait_items.iter().flat_map(|ii| ii.to_clauses(program)))
            .collect()
    }
}

impl TraitItem {
    pub fn to_clauses(&self, program: &crate::grammar::Program) -> Vec<ProgramClause> {}
}
