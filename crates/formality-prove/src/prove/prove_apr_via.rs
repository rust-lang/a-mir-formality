use formality_macros::term;
use formality_types::{
    cast::Upcast,
    collections::Set,
    grammar::{WcData, Wcs, APR},
    judgment,
    judgment::Judgment,
};

use crate::{
    program::Program,
    prove::{prove_after::prove_after, prove_eq::prove_parameters_eq},
};

use super::ConstraintSet;

pub fn prove_apr_via(
    program: impl Upcast<Program>,
    assumptions: impl Upcast<Wcs>,
    via: impl Upcast<WcData>,
    goal: impl Upcast<APR>,
) -> Set<ConstraintSet> {
    ProveAprVia(
        program.upcast(),
        assumptions.upcast(),
        via.upcast(),
        goal.upcast(),
    )
    .apply()
}

#[term]
struct ProveAprVia(Program, Wcs, WcData, APR);

judgment! {
    (ProveAprVia => ConstraintSet)

    (
        (let (skel_c, parameters_c) = clause.debone())
        (let (skel_g, parameters_g) = goal.debone())
        (if skel_c == skel_g)
        (prove_parameters_eq(program, assumptions, parameters_c, parameters_g) => c)
        ----------------------------- ("axiom")
        (ProveAprVia(program, assumptions, WcData::Atomic(clause), goal) => c)
    )

    (
        (let via1 = binder.instantiate_existentially((&assumptions, &goal)))
        (prove_apr_via(program, assumptions, via1, goal) => c)
        ----------------------------- ("forall")
        (ProveAprVia(program, assumptions, WcData::ForAll(binder), goal) => c)
    )

    (
        (prove_apr_via(&program, &assumptions, wc_consequence, goal) => c1)
        (prove_after(&program, &assumptions, c1, &wc_condition) => c2)
        ----------------------------- ("implies")
        (ProveAprVia(program, assumptions, WcData::Implies(wc_condition, wc_consequence), goal) => c2)
    )
}
