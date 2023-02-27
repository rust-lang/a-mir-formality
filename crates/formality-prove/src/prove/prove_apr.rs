use formality_macros::term;
use formality_types::{
    cast::Upcast,
    collections::Set,
    grammar::{AtomicRelation, Parameter, WcList, APR},
    judgment,
    judgment::Judgment,
};

use crate::{
    program::Program,
    prove::{prove_apr_via::prove_apr_via, prove_eq::prove_ty_eq},
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

    // (
    //     -----------------------------
    //     (ProveApr(program, assumptions, APR::AtomicPredicate(AtomicPredicate::IsImplemented(trait_ref))) => c)
    // )

    (
        (prove_ty_eq(program, assumptions, a, b) => c)
        -----------------------------
        (ProveApr(program, assumptions, APR::AtomicRelation(AtomicRelation::Equals(Parameter::Ty(a), Parameter::Ty(b)))) => c)
    )
}
