use formality_types::{
    grammar::{WcData, Wcs, APR},
    judgment_fn,
};

use crate::{
    program::Program,
    prove::{
        constraints::{no_constraints, Constraints},
        env::Env,
        prove,
        prove_after::prove_after,
        prove_eq::all_eq,
    },
};

judgment_fn! {
    pub fn prove_apr_via(
        program: Program,
        env: Env,
        assumptions: Wcs,
        via: WcData,
        goal: APR,
    ) => (Env, Constraints) {
        (
            (let (skel_c, parameters_c) = predicate.debone())
            (let (skel_g, parameters_g) = goal.debone())
            (if skel_c == skel_g)
            (prove(program, env, assumptions, all_eq(parameters_c, parameters_g)) => (env, c))
            ----------------------------- ("predicate-congruence-axiom")
            (prove_apr_via(program, env, assumptions, APR::AtomicPredicate(predicate), goal) => (env, c))
        )

        (
            (let (skel_c, parameters_c) = relation.debone())
            (let (skel_g, parameters_g) = goal.debone())
            (if skel_c == skel_g)
            (if parameters_c == parameters_g) // for relations, we require 100% match
            ----------------------------- ("relation-axiom")
            (prove_apr_via(_program, env, _assumptions, APR::AtomicRelation(relation), goal) => (env, no_constraints()))
        )

        (
            (let (env, subst) = env.existential_substitution(&binder))
            (let via1 = binder.instantiate_with(&subst).unwrap())
            (prove_apr_via(program, env, assumptions, via1, goal) => (env, c))
            (let (env, c) = c.pop_subst(env, &subst))
            ----------------------------- ("forall")
            (prove_apr_via(program, env, assumptions, WcData::ForAll(binder), goal) => (env, c))
        )

        (
            (prove_apr_via(&program, env, &assumptions, wc_consequence, goal) => (env, c))
            (prove_after(&program, env, c, &assumptions, &wc_condition) => (env, c))
            ----------------------------- ("implies")
            (prove_apr_via(program, env, assumptions, WcData::Implies(wc_condition, wc_consequence), goal) => (env, c))
        )
    }
}
