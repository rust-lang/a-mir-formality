use crate::grammar::{Lt, Predicate, RigidName, RigidTy, Ty, Wc, Wcs};
use formality_core::{judgment_fn, visit::CoreVisit, Downcast};

use crate::prove::{
    constraints::Constraints, decls::Program, env::Env, prove, prove_after::prove_after,
    prove_outlives::prove_outlives_bound,
};

judgment_fn! {
    /// Check whether the where-clause `via` (which is one of the `assumptions` that are in in scope)
    /// can be used to prove `goal` (the thing we are trying to prove).
    ///
    /// This is equivalent to the "elaboration" of the environment that takes place in rustc,
    /// but done lazilly. For example, if you have `where T: Eq` then you can clearly prove `T: Eq`
    /// but you can also prove `T: PartialEq` because `trait Eq: PartialEq`.
    pub fn prove_via(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        via: Wc,
        goal: Wc,
    ) => Constraints {
        debug(goal, via, assumptions, env)

        (
            (if let Some(RigidTy { name: RigidName::Ref(_), parameters }) = source.downcast())
            (let (region, ty) = parameters.downcast_err::<(Lt, Ty)>()?)
            (prove_outlives_bound(decls, env, assumptions, ty, region, target, target_region) => c)
            ----------------------------- ("well-formed reference input")
            (prove_via(decls, env, assumptions, Predicate::WellFormed(source), Predicate::Outlives(target, target_region)) => c)
        )

        (
            (prove_outlives_bound(decls, env, assumptions, source, source_region, target, target_region) => c)
            ----------------------------- ("outlives bound")
            (prove_via(decls, env, assumptions, Predicate::Outlives(source, source_region), Predicate::Outlives(target, target_region)) => c)
        )

        (
            (if source.trait_id == target.trait_id)!
            (prove(decls, env, assumptions, Wcs::all_eq(&source.parameters, &target.parameters)) => c)
            ----------------------------- ("coinductive hypothesis")
            (prove_via(decls, env, assumptions, Wc::Coinductive(source), Predicate::IsImplemented(target)) => c)
        )

        (
            (bound in decls.trait_bounds(source))
            (if (env, source).size() <= decls.max_size)!
            (prove_via(decls, env, assumptions.without_coinductive(), bound, goal) => c)
            ----------------------------- ("trait declaration")
            (prove_via(decls, env, assumptions, Predicate::IsImplemented(source), goal) => c)
        )

        (
            (_bound in decls.trait_bounds(source))
            (if (env, source).size() > decls.max_size)!
            ----------------------------- ("trait declaration overflow")
            (prove_via(decls, env, assumptions, Predicate::IsImplemented(source), goal) => Constraints::none(env).ambiguous())
        )

        (
            // `c` = "clause", the name for something that we are assuming is true.
            (let (skel_c, parameters_c) = pred_1.debone())
            // `g` = "goal, the name for something that we are trying to prove.
            (let (skel_g, parameters_g) = pred_2.debone())
            (if !skel_c.is_relation())
            (if skel_c == skel_g)!
            (prove(decls, env, assumptions, Wcs::all_eq(parameters_c, parameters_g)) => c)
            ----------------------------- ("predicate-congruence-axiom")
            (prove_via(decls, env, assumptions, Wc::Predicate(pred_1), Wc::Predicate(pred_2)) => c)
        )

        (
            (let (skel_c, parameters_c) = rel_1.debone())
            (let (skel_g, parameters_g) = rel_2.debone())
            (if skel_c.is_relation())
            (if skel_c == skel_g)
            (if parameters_c == parameters_g)! // for relations, we require 100% match
            ----------------------------- ("relation-axiom")
            (prove_via(_decls, env, _assumptions, Wc::Predicate(rel_1), Wc::Predicate(rel_2)) => Constraints::none(env))
        )

        // If you have `where for<'a> T: Trait<'a>` then you can prove `T: Trait<'b>` for any `'b`.
        (
            (let (env, subst) = env.existential_substitution(binder))
            (let via1 = binder.instantiate_with(&subst).unwrap())
            // Try to prove `T: Trait<?a> == goal`.
            (prove_via(decls, env, assumptions, via1, goal) => c)
            ----------------------------- ("forall")
            (prove_via(decls, env, assumptions, Wc::ForAll(binder), goal) => c.pop_subst(&subst))
        )

        // If you have `where if (T: Debug) T: Foo` (not in Rust but it should be...)...
        (
            // if the goal is `T: Foo`...
            (prove_via(decls, env, assumptions, wc_consequence, goal) => c)
            // ...and we can prove `T: Debug`... then it holds.
            (prove_after(decls, c, assumptions, wc_condition) => c)
            ----------------------------- ("implies")
            (prove_via(decls, env, assumptions, Wc::Implies(wc_condition, wc_consequence), goal) => c)
        )
    }
}
