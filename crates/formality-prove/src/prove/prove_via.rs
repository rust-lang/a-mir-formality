use formality_core::judgment_fn;
use formality_types::grammar::{WcData, Wcs};

use crate::{
    decls::Decls,
    prove::{constraints::Constraints, env::Env, prove, prove_after::prove_after},
};

judgment_fn! {
    pub fn prove_via(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        via: WcData,
        goal: WcData,
    ) => Constraints {
        debug(goal, via, assumptions, env, decls)

        (
            (let (skel_c, parameters_c) = pred_1.debone())
            (let (skel_g, parameters_g) = pred_2.debone())
            (if skel_c == skel_g)!
            (prove(decls, env, assumptions, Wcs::all_eq(parameters_c, parameters_g)) => c)
            ----------------------------- ("predicate-congruence-axiom")
            (prove_via(decls, env, assumptions, WcData::Predicate(pred_1), WcData::Predicate(pred_2)) => c)
        )

        (
            (let (skel_c, parameters_c) = rel_1.debone())
            (let (skel_g, parameters_g) = rel_2.debone())
            (if skel_c == skel_g)
            (if parameters_c == parameters_g)! // for relations, we require 100% match
            ----------------------------- ("relation-axiom")
            (prove_via(_decls, env, _assumptions, WcData::Relation(rel_1), WcData::Relation(rel_2)) => Constraints::none(env))
        )

        (
            (let (env, subst) = env.existential_substitution(&binder))
            (let via1 = binder.instantiate_with(&subst).unwrap())
            (prove_via(decls, env, assumptions, via1, goal) => c)
            ----------------------------- ("forall")
            (prove_via(decls, env, assumptions, WcData::ForAll(binder), goal) => c.pop_subst(&subst))
        )

        (
            (prove_via(&decls, env, &assumptions, wc_consequence, goal) => c)
            (prove_after(&decls, c, &assumptions, &wc_condition) => c)
            ----------------------------- ("implies")
            (prove_via(decls, env, assumptions, WcData::Implies(wc_condition, wc_consequence), goal) => c)
        )
    }
}
