use formality_types::{
    grammar::{WcData, Wcs, PR},
    judgment_fn,
};

use crate::{
    decls::Decls,
    prove::{
        constraints::Constraints, env::Env, prove, prove_after::prove_after, prove_eq::all_eq,
    },
};

judgment_fn! {
    pub fn prove_pr_via(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        via: WcData,
        goal: PR,
    ) => Constraints {
        debug(goal, via, assumptions, env, decls)

        (
            (let (skel_c, parameters_c) = predicate.debone())
            (let (skel_g, parameters_g) = goal.debone())
            (if skel_c == skel_g)
            (prove(decls, env, assumptions, all_eq(parameters_c, parameters_g)) => c)
            ----------------------------- ("predicate-congruence-axiom")
            (prove_pr_via(decls, env, assumptions, PR::Predicate(predicate), goal) => c)
        )

        (
            (let (skel_c, parameters_c) = relation.debone())
            (let (skel_g, parameters_g) = goal.debone())
            (if skel_c == skel_g)
            (if parameters_c == parameters_g) // for relations, we require 100% match
            ----------------------------- ("relation-axiom")
            (prove_pr_via(_decls, env, _assumptions, PR::Relation(relation), goal) => Constraints::none(env))
        )

        (
            (let (env, subst) = env.existential_substitution(&binder))
            (let via1 = binder.instantiate_with(&subst).unwrap())
            (prove_pr_via(decls, env, assumptions, via1, goal) => c)
            ----------------------------- ("forall")
            (prove_pr_via(decls, env, assumptions, WcData::ForAll(binder), goal) => c.pop_subst(&subst))
        )

        (
            (prove_pr_via(&decls, env, &assumptions, wc_consequence, goal) => c)
            (prove_after(&decls, c, &assumptions, &wc_condition) => c)
            ----------------------------- ("implies")
            (prove_pr_via(decls, env, assumptions, WcData::Implies(wc_condition, wc_consequence), goal) => c)
        )
    }
}
