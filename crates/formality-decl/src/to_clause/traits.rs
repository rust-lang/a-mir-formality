use formality_types::grammar::{Hypothesis, Invariant, ProgramClause, TraitRef};

use crate::grammar::{Trait, TraitBoundData, TraitItem};

impl Trait {
    pub fn to_clauses(&self, program: &crate::grammar::Program) -> Vec<ProgramClause> {
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
            Hypothesis::implies(where_clauses, trait_ref.well_formed()),
        );

        std::iter::once(is_implemented)
            .chain(Some(well_formed))
            .chain(trait_items.iter().flat_map(|ii| ii.to_clauses(program)))
            .collect()
    }

    pub fn invariants(&self, program: &crate::grammar::Program) -> Vec<Invariant> {
        let (
            trait_kinded_var_ids,
            TraitBoundData {
                where_clauses,
                trait_items,
            },
        ) = self.binder.open();

        let trait_ref = TraitRef::new(&self.id, &trait_kinded_var_ids);

        vec![]
    }
}

impl TraitItem {
    pub fn to_clauses(&self, _program: &crate::grammar::Program) -> Vec<ProgramClause> {
        unimplemented!()
    }
}
