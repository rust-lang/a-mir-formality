use formality_macros::term;
use formality_types::{
    collections::Set,
    grammar::{AtomicRelation, Parameter, WcList, APR},
    judgment,
    judgment::Judgment,
};

use crate::{
    env::Env,
    prove::{prove_apr_via::prove_apr_via, prove_eq::prove_ty_eq},
};

use super::ConstraintSet;

pub fn prove_apr(env: Env, assumptions: WcList, goal: APR) -> Set<ConstraintSet> {
    ProveApr(env, assumptions.clone(), goal).apply()
}

#[term]
struct ProveApr(Env, WcList, APR);

judgment! {
    (ProveApr => ConstraintSet)

    (
        (assumptions.iter() => a)
        (prove_apr_via(env, &assumptions, a, &goal) => c)
        -----------------------------
        (ProveApr(env, assumptions, goal) => c)
    )

    // (
    //     -----------------------------
    //     (ProveApr(env, assumptions, APR::AtomicPredicate(AtomicPredicate::IsImplemented(trait_ref))) => c)
    // )

    (
        (prove_ty_eq(env, assumptions, a, b) => c)
        -----------------------------
        (ProveApr(env, assumptions, APR::AtomicRelation(AtomicRelation::Equals(Parameter::Ty(a), Parameter::Ty(b)))) => c)
    )
}
