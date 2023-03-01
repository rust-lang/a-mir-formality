use formality_types::{
    grammar::{WcData, Wcs, APR},
    judgment_fn,
};

use crate::{
    program::Program,
    prove::{prove_after::prove_after, prove_eq::prove_parameters_eq},
};

use super::ConstraintSet;

judgment_fn! {
    pub fn prove_apr_via(
        program: Program,
        assumptions: Wcs,
        via: WcData,
        goal: APR,
    ) => ConstraintSet {
        (
            (let (skel_c, parameters_c) = clause.debone())
            (let (skel_g, parameters_g) = goal.debone())
            (if skel_c == skel_g)
            (prove_parameters_eq(program, assumptions, parameters_c, parameters_g) => c)
            ----------------------------- ("axiom")
            (prove_apr_via(program, assumptions, WcData::Atomic(clause), goal) => c)
        )

        (
            (let via1 = binder.instantiate_existentially((&assumptions, &goal)))
            (prove_apr_via(program, assumptions, via1, goal) => c)
            ----------------------------- ("forall")
            (prove_apr_via(program, assumptions, WcData::ForAll(binder), goal) => c)
        )

        (
            (prove_apr_via(&program, &assumptions, wc_consequence, goal) => c1)
            (prove_after(&program, &assumptions, c1, &wc_condition) => c2)
            ----------------------------- ("implies")
            (prove_apr_via(program, assumptions, WcData::Implies(wc_condition, wc_consequence), goal) => c2)
        )
}
}
