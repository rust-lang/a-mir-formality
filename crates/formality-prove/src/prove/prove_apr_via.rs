use formality_types::{
    grammar::{Binder, WcData, Wcs, APR},
    judgment_fn,
};

use crate::{
    program::Program,
    prove::{
        constraints::{merge_constraints, Constraints},
        prove_after::prove_after,
        prove_eq::prove_parameters_eq,
        subst::existential_substitution,
    },
};

judgment_fn! {
    pub fn prove_apr_via(
        program: Program,
        assumptions: Wcs,
        via: WcData,
        goal: APR,
    ) => Binder<Constraints> {
        (
            (let (skel_c, parameters_c) = clause.debone())
            (let (skel_g, parameters_g) = goal.debone())
            (if skel_c == skel_g)
            (prove_parameters_eq(program, assumptions, parameters_c, parameters_g) => c)
            ----------------------------- ("axiom")
            (prove_apr_via(program, assumptions, WcData::Atomic(clause), goal) => c)
        )

        (
            (let subst = existential_substitution(&binder, (&assumptions, &goal)))
            (let via1 = binder.instantiate_with(&subst).unwrap())
            (prove_apr_via(program, assumptions, via1, goal) => c)
            ----------------------------- ("forall")
            (prove_apr_via(program, assumptions, WcData::ForAll(binder), goal) => merge_constraints(&subst, (), c))
        )

        (
            (prove_apr_via(&program, &assumptions, wc_consequence, goal) => c1)
            (prove_after(&program, c1, &assumptions, &wc_condition) => c2)
            ----------------------------- ("implies")
            (prove_apr_via(program, assumptions, WcData::Implies(wc_condition, wc_consequence), goal) => c2)
        )
}
}
