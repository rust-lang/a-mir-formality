use contracts::requires;
use formality_types::{
    cast::Upcast,
    grammar::{
        AtomicPredicate, AtomicRelation, Hypothesis, Invariant, InvariantImplication,
        KindedVarIndex, Parameter, ParameterKind, Predicate, PredicateData, ProgramClause,
        TraitRef, APR,
    },
};

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
                where_clauses,
                trait_items: _,
            },
        ) = self.binder.open();

        let trait_ref = TraitRef::new(&self.id, &trait_kinded_var_ids);
        let is_implemented = trait_ref.is_implemented();

        superbounds(&trait_kinded_var_ids, &where_clauses)
            .into_iter()
            .map(|superbound| {
                Invariant::for_all(
                    &trait_kinded_var_ids,
                    InvariantImplication::new(&is_implemented, superbound),
                )
            })
            .collect()
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

#[requires(trait_kinded_var_ids[0].kind == ParameterKind::Ty)]
fn superbounds(
    trait_kinded_var_ids: &[KindedVarIndex],
    where_clauses: &[Predicate],
) -> Vec<Predicate> {
    let self_ty: Parameter = trait_kinded_var_ids[0].upcast();

    where_clauses
        .iter()
        .filter(|p| is_superpredicate(&self_ty, p))
        .cloned()
        .collect()
}

fn is_superpredicate(self_ty: &Parameter, predicate: &Predicate) -> bool {
    match predicate.data() {
        PredicateData::Atomic(APR::AtomicPredicate(AtomicPredicate::IsImplemented(trait_ref))) => {
            trait_ref.parameters[0] == *self_ty
        }
        PredicateData::Atomic(APR::AtomicPredicate(AtomicPredicate::NormalizesTo(alias_ty, _))) => {
            alias_ty.parameters[0] == *self_ty
        }
        PredicateData::Atomic(APR::AtomicRelation(AtomicRelation::Outlives(a, _))) => {
            *a == *self_ty
        }

        // Users can't write these, so we don't expect them to appear on the trait where-clauses list.
        // If that changes, we can decide whether they should be considered a super predicate.
        PredicateData::Atomic(APR::AtomicPredicate(AtomicPredicate::HasImpl(..)))
        | PredicateData::Atomic(APR::AtomicPredicate(AtomicPredicate::WellFormedTy(..)))
        | PredicateData::Atomic(APR::AtomicPredicate(AtomicPredicate::WellFormedTraitRef(..)))
        | PredicateData::Atomic(APR::AtomicRelation(AtomicRelation::Sub(..)))
        | PredicateData::Atomic(APR::AtomicRelation(AtomicRelation::Equals(..))) => {
            panic!("unexepected relation in the trait super-predicates list")
        }

        PredicateData::ForAll(p) => is_superpredicate(self_ty, p.peek()),
        PredicateData::Implies(_, p) => is_superpredicate(self_ty, p),
    }
}
