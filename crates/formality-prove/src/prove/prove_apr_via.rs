use formality_types::{
    grammar::{Binder, WcData, Wcs, APR},
    judgment_fn,
};

use crate::{
    program::Program,
    prove::{
        constraints::{merge_constraints, no_constraints, Constraints},
        prove,
        prove_after::prove_after,
        prove_eq::all_eq,
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
            (let (skel_c, parameters_c) = predicate.debone())
            (let (skel_g, parameters_g) = goal.debone())
            (if skel_c == skel_g)
            (prove(program, assumptions, all_eq(parameters_c, parameters_g)) => c)
            ----------------------------- ("predicate-congruence-axiom")
            (prove_apr_via(program, assumptions, APR::AtomicPredicate(predicate), goal) => c)
        )

        (
            (let (skel_c, parameters_c) = relation.debone())
            (let (skel_g, parameters_g) = goal.debone())
            (if skel_c == skel_g)
            (if parameters_c == parameters_g) // for relations, we require 100% match
            ----------------------------- ("relation-axiom")
            (prove_apr_via(_program, _assumptions, APR::AtomicRelation(relation), goal) => no_constraints(()))
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
