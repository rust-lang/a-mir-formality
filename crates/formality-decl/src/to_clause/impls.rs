use formality_types::grammar::{BoundVar, Hypothesis, Predicate, ProgramClause, TraitRef};

use crate::grammar::{ImplItem, Program, TraitImpl, TraitImplBoundData};

impl TraitImpl {
    pub(super) fn to_clauses(&self, program: &Program) -> Vec<ProgramClause> {
        let (
            impl_bound_vars,
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
            &impl_bound_vars,
            Hypothesis::implies(&where_clauses, trait_ref.has_impl()),
        );

        std::iter::once(has_impl)
            .chain(impl_items.iter().flat_map(|ii| {
                ii.to_clauses(&impl_bound_vars, &trait_ref, &where_clauses, program)
            }))
            .collect()
    }
}

impl ImplItem {
    pub fn to_clauses(
        &self,
        impl_bound_vars: &[BoundVar],
        trait_ref: &TraitRef,
        impl_where_clauses: &[Predicate],
        program: &Program,
    ) -> Vec<ProgramClause> {
        match self {
            ImplItem::Fn(v) => v.to_clauses(impl_bound_vars, program),
            ImplItem::AssociatedTyValue(v) => {
                v.to_clauses(impl_bound_vars, trait_ref, impl_where_clauses, program)
            }
        }
    }
}
