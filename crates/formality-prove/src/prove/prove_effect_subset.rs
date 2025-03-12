use formality_core::{judgment_fn, ProvenSet, Set, set};
use formality_types::grammar::{AtomicEffect, Effect, Wcs};
use std::fmt::Debug;

use crate::{prove::prove_eq::prove_traitref_eq, Decls};

use super::{Constraints, Env};


judgment_fn! {
    /// Prove an effect is the subset of another effect. 
    pub fn prove_effect_subset(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        subset: Effect,
        superset: Effect,
    ) => Constraints {
        debug(subset, superset, assumptions, env)
        trivial(subset == superset => Constraints::none(env))

        // (A union B) is a subset of C if
        // * A is a subset of C and
        // * B is a subset of C
        (
            // example: imagine that `subset = union({<?T as Trait>::Effect}, {<?T as Trait>::Effect})`, and then
            //
            // * subset1 = `{<?T as Trait>::Effect}` and
            // * superset = `{<u32 as Trait>::Effect, <i32 as Trait>::Effect)`
            //
            // this is provable in two ways (and hence will yield two results)
            //
            // * first, with c1 = [?T = u32]
            // * later, with c1 = [?T = i32]
            (prove_effect_subset(&decls, env, &assumptions, &*subset1, &superset) => c1)

            // replace any inference variables in `subset2` that got constrained
            // with the value they were forced to equal
            //
            // continuing the example, imagine
            //
            // * subset2 = `{<?T as Trait>::Effect}`
            //
            // with the first value of `c1`, we will substitute and get
            //
            // * subset2 = `{u32 as Trait>::Effect}`
            //
            // and we will then try to prove it (which succeeds with no constraints).
            //
            // in second iteration, we will get `<i32 as Trait>::Effect`, again provable.
            (let subset2 = c1.substitution().apply(&subset2))

            // prove that this refined version of `subset2` is true
            (prove_effect_subset(&decls, c1.env(), &assumptions, &*subset2, &superset) => c2)
            --- ("union")
            (prove_effect_subset(decls, env, assumptions, Effect::Union(subset1, subset2), superset) => c1.seq(c2))
        )

        // If `subset` is an atomic effect, then use the `prove_atomic_effect_subset` rule
        (
            (prove_atomic_effect_subset(&decls, env, assumptions, subeffect, superset) => constraints)
            --- ("atomic")
            (prove_effect_subset(decls, env, assumptions, Effect::Atomic(subeffect), superset) => constraints)
        )
    }
}


judgment_fn! {
    /// Prove that an atomic effect appears in `superset`.
    /// Search for any atomic element of superset and check that it is equal.
    fn prove_atomic_effect_subset(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        atomic_subeffect: AtomicEffect,
        superset: Effect,
    ) => Constraints {
        debug(atomic_subeffect, superset, assumptions, env)

        (
            // find some atomic effect in the superset...
            (some_atomic_effect(superset) => supereffect)
            // ...and prove it is equal to the atomic from the subset
            (prove_atomic_effect_eq(&decls, &env, &assumptions, &atomic_subeffect, supereffect) => constraints)
            --- ("union-subset-lhs")
            (prove_atomic_effect_subset(decls, env, assumptions, atomic_subeffect, superset) => constraints)
        )
    }
}


judgment_fn! {
    /// Prove two atomic effects are equal. 
    fn prove_atomic_effect_eq(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        f1: AtomicEffect,
        f2: AtomicEffect,
    ) => Constraints {
        debug(f1, f2, assumptions, env)
        trivial(f1 == f2 => Constraints::none(env))

        (
            (prove_traitref_eq(decls, env, assumptions, &*tr1, &*tr2) => constraints)
            --- ("associated-effect")
            (prove_atomic_effect_eq(decls, env, assumptions, AtomicEffect::AssociatedEffect(tr1), AtomicEffect::AssociatedEffect(tr2)) => constraints)
        )
    }
}

/// Proves judgment for each of the given items.
pub fn collect<T: Ord + Debug>(judgment: ProvenSet<T>) -> ProvenSet<Set<T>> {
    match judgment.into_set() {
        Ok(s) => ProvenSet::proven(set![s]),
        Err(e) => ProvenSet::from(*e),
    }
}

judgment_fn! {
    /// Get any atomic effect from an Effect::Union. 
    fn some_atomic_effect(
        f1: Effect
    ) => AtomicEffect {
        debug(f1)

        (
            (if let Effect::Atomic(e) = *f1)!
            --- ("union-lhs")
            (some_atomic_effect(Effect::Union(f1, _f2)) => e)
        )

        (
            (if let Effect::Atomic(e) = *f2)!
            --- ("union-rhs")
            (some_atomic_effect(Effect::Union(_f1, f2)) => e)
        )

        (
            --- ("atomic")
            (some_atomic_effect(Effect::Atomic(e)) => e)
        )
    }
}