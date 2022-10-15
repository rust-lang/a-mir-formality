use formality_core::all_into::AllInto;
use formality_types::grammar::{Binder, HypothesisData, ProgramClause};

use crate::grammar::{TraitImpl, TraitImplBoundData};

use super::ToClause;

impl ToClause for TraitImpl {
    fn to_clauses(&self, program: &crate::grammar::Program) -> Vec<ProgramClause> {
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
        // ∀. is_implemented(TraitRef) :- where_clauses
        let is_implemented = Hypothesis::for_all(
            &impl_kinded_var_ids,
            Hypothesis::implies(where_clauses, trait_ref),
        );

        vec![is_implemented]
    }
}
