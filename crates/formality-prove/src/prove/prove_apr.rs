use formality_macros::term;
use formality_types::{
    cast::Upcast,
    collections::Set,
    grammar::{AtomicPredicate, AtomicRelation, Parameter, WcList, APR},
    judgment,
    judgment::Judgment,
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
    assumptions: impl Upcast<WcList>,
    goal: impl Upcast<APR>,
) -> Set<ConstraintSet> {
    ProveApr(program.upcast(), assumptions.upcast(), goal.upcast()).apply()
}

#[term]
struct ProveApr(Program, WcList, APR);

judgment! {
    (ProveApr => ConstraintSet)

    (
        (assumptions.iter() => a)
        (prove_apr_via(&program, &assumptions, a, &goal) => c)
        -----------------------------
        (ProveApr(program, assumptions, goal) => c)
    )

    (
        (program.impl_decls(&trait_ref.trait_id) => i)
        (let i = i.binder.instantiate_existentially((&assumptions, &trait_ref)))
        (let g_eq = all_eq(&trait_ref.parameters, &i.trait_ref.parameters))
        (let assumptions_c = assumptions.clone().and(trait_ref.is_implemented()))
        (prove_wc_list(&program, &assumptions_c, g_eq.and(i.where_clause)) => c)
        -----------------------------
        (ProveApr(program, assumptions, APR::AtomicPredicate(AtomicPredicate::IsImplemented(trait_ref))) => c)
    )

    // (
    //     (let t = program.trait_decl(&trait_ref.trait_id))
    //     (let t = t.binder.instantiate_existentially((&assumptions, &trait_ref)))
    //     (prove_apr_via(&program, &assumptions, t.where_clause, trait_ref.is_implemented()) => c1)
    //     (prove_after(&program, &assumptions, c1, t.trait_ref()) => c2)
    //     -----------------------------
    //     (ProveApr(program, assumptions, APR::AtomicPredicate(AtomicPredicate::IsImplemented(trait_ref))) => c2)
    // )

    (
        (prove_ty_eq(program, assumptions, a, b) => c)
        -----------------------------
        (ProveApr(program, assumptions, APR::AtomicRelation(AtomicRelation::Equals(Parameter::Ty(a), Parameter::Ty(b)))) => c)
    )
}
