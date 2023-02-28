use formality_macros::term;
use formality_types::{
    cast::Upcast,
    collections::Set,
    grammar::{AtomicPredicate, AtomicRelation, Parameter, Wcs, APR},
    judgment,
    judgment::Judgment,
    set,
};

use crate::{
    program::Program,
    prove::{
        prove_after::prove_after,
        prove_apr_via::prove_apr_via,
        prove_eq::{all_eq, prove_ty_eq},
        prove_wc_list::prove_wc_list,
    },
};

use super::ConstraintSet;

pub fn prove_apr(
    program: impl Upcast<Program>,
    assumptions: impl Upcast<Wcs>,
    goal: impl Upcast<APR>,
) -> Set<ConstraintSet> {
    ProveApr(program.upcast(), assumptions.upcast(), goal.upcast()).apply()
}

#[term]
struct ProveApr(Program, Wcs, APR);

judgment! {
    (ProveApr => ConstraintSet)

    (
        (&assumptions => a)
        (prove_apr_via(&program, &assumptions, a, &goal) => c)
        -----------------------------
        (ProveApr(program, assumptions, goal) => c)
    )

    (
        (program.impl_decls(&trait_ref.trait_id) => i)
        (let i = i.binder.instantiate_existentially((&assumptions, &trait_ref)))
        (let t = program.trait_decl(&i.trait_ref.trait_id).binder.instantiate_with(&i.trait_ref.parameters).unwrap())
        (let assumptions_c = assumptions.union(trait_ref.is_implemented()))
        (prove_wc_list(&program, &assumptions_c, set![
            ..all_eq(&trait_ref.parameters, &i.trait_ref.parameters),
            ..i.where_clause,
            ..t.where_clause,
        ]) => c)
        -----------------------------
        (ProveApr(program, assumptions, APR::AtomicPredicate(AtomicPredicate::IsImplemented(trait_ref))) => c)
    )

    (
        (let t = program.trait_decl(&trait_ref.trait_id))
        (let (t, t_wcs) = t.trait_ref_and_wc().instantiate_existentially((&assumptions, &trait_ref)))
        (t_wcs => t_wc)
        (prove_apr_via(&program, &assumptions, &t_wc, trait_ref.is_implemented()) => c1)
        (prove_after(&program, &assumptions, c1, t.is_implemented()) => c2)
        -----------------------------
        (ProveApr(program, assumptions, APR::AtomicPredicate(AtomicPredicate::IsImplemented(trait_ref))) => c2)
    )

    (
        (prove_ty_eq(program, assumptions, a, b) => c)
        -----------------------------
        (ProveApr(program, assumptions, APR::AtomicRelation(AtomicRelation::Equals(Parameter::Ty(a), Parameter::Ty(b)))) => c)
    )
}
