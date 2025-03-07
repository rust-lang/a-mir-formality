use formality_core::judgment_fn;
use formality_types::grammar::{DebonedPredicate, WcData, Wcs};

use crate::{
    decls::Decls,
    prove::{constraints::Constraints, env::Env, prove, prove_after::prove_after},
};

judgment_fn! {
    /// Check whether the where-clause `via` (which is one of the `assumptions` that are in in scope)
    /// can be used to prove `goal` (the thing we are trying to prove).
    ///
    /// This is equivalent to the "elaboration" of the environment that takes place in rustc,
    /// but done lazilly. For example, if you have `where T: Eq` then you can clearly prove `T: Eq`
    /// but you can also prove `T: PartialEq` because `trait Eq: PartialEq`.
    pub fn prove_via(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        via: WcData,
        goal: WcData,
    ) => Constraints {
        debug(goal, via, assumptions, env)

        (
            // `c` = "clause", the name for something that we are assuming is true
            (let DebonedPredicate { skeleton: skel_c, parameters: parameters_c, effects: effects_c } = pred_c.debone())
            // `g` = "goal, the name for something that we are trying to prove
            (let DebonedPredicate { skeleton: skel_g, parameters: parameters_g, effects: effects_g } = pred_g.debone())
            (if skel_c == skel_g)!
            (prove(decls, env, assumptions, Wcs::all_eq(parameters_c, parameters_g)) => c)
            (if effects_c == effects_g) // FIXME: this is not general enough but we will revisit it
            ----------------------------- ("predicate-congruence-axiom")
            (prove_via(decls, env, assumptions, WcData::Predicate(pred_c), WcData::Predicate(pred_g)) => c)
        )

        (
            (let DebonedPredicate { skeleton: skel_c, parameters: parameters_c, effects: effects_c } = rel_c.debone())
            (let DebonedPredicate { skeleton: skel_g, parameters: parameters_g, effects: effects_g } = rel_g.debone())
            (if skel_c == skel_g)
            // for relations, we require 100% match
            (if parameters_c == parameters_g)
            (if effects_c == effects_g)!
            ----------------------------- ("relation-axiom")
            (prove_via(_decls, env, _assumptions, WcData::Relation(rel_c), WcData::Relation(rel_g)) => Constraints::none(env))
        )

        // If you have `where for<'a> T: Trait<'a>` then you can prove `T: Trait<'b>` for any `'b`
        (
            // replace `'a` with an existential variable `?a`
            (let (env, subst) = env.existential_substitution(&binder))
            (let via1 = binder.instantiate_with(&subst).unwrap())

            // try to prove `T: Trait<?a> == goal`
            (prove_via(decls, env, assumptions, via1, goal) => c)
            ----------------------------- ("forall")
            (prove_via(decls, env, assumptions, WcData::ForAll(binder), goal) => c.pop_subst(&subst))
        )

        // If you have `where if (T: Debug) T: Foo` (not in Rust but it should be...)...
        (
            // if the goal is `T: Foo`...
            (prove_via(&decls, env, &assumptions, wc_consequence, goal) => c)
<<<<<<< HEAD
=======

>>>>>>> 1c31da9 (comments + newtype for DebonedPredicate)
            // ...and we can prove `T: Debug`... then it holds.
            (prove_after(&decls, c, &assumptions, &wc_condition) => c)
            ----------------------------- ("implies")
            (prove_via(decls, env, assumptions, WcData::Implies(wc_condition, wc_consequence), goal) => c)
        )
    }
}
