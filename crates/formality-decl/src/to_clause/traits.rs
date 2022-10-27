use formality_types::grammar::{Hypothesis, Invariant, KindedVarIndex, ProgramClause, TraitRef};

use crate::grammar::{Trait, TraitBoundData, TraitItem};

impl Trait {
    pub(super) fn to_clauses(&self, program: &crate::grammar::Program) -> Vec<ProgramClause> {
        let (
            trait_kinded_var_ids,
            TraitBoundData {
                where_clauses,
                trait_items,
            },
        ) = self.binder.open();

        let trait_ref = TraitRef::new(&self.id, &trait_kinded_var_ids);

        // Main rule 1 for the trait:
        //
        // ∀. is_implemented(TraitRef) :- has_impl(TraitRef), well_formed(TraitRef)
        let is_implemented = Hypothesis::for_all(
            &trait_kinded_var_ids,
            Hypothesis::implies(
                vec![trait_ref.has_impl(), trait_ref.well_formed()],
                trait_ref.is_implemented(),
            ),
        );

        // Main rule 2 for the trait:
        //
        // ∀. well_formed(TraitRef) :- ...where_clauses...
        let well_formed = Hypothesis::for_all(
            &trait_kinded_var_ids,
            Hypothesis::implies(&where_clauses, trait_ref.well_formed()),
        );

        std::iter::once(is_implemented)
            .chain(Some(well_formed))
            .chain(
                trait_items
                    .iter()
                    .flat_map(|ii| ii.to_clauses(&trait_kinded_var_ids, program)),
            )
            .collect()
    }

    pub(super) fn to_invariants(&self, _program: &crate::grammar::Program) -> Vec<Invariant> {
        let (
            trait_kinded_var_ids,
            TraitBoundData {
                where_clauses: _,
                trait_items: _,
            },
        ) = self.binder.open();

        let _trait_ref = TraitRef::new(&self.id, &trait_kinded_var_ids);

        vec![]
    }
}

impl TraitItem {
    pub(super) fn to_clauses(
        &self,
        trait_kinded_var_ids: &[KindedVarIndex],
        program: &crate::grammar::Program,
    ) -> Vec<ProgramClause> {
        match self {
            TraitItem::Fn(f) => f.to_clauses(trait_kinded_var_ids, program),
            TraitItem::AssociatedTy(v) => v.to_clauses(trait_kinded_var_ids, program),
        }
    }
}
