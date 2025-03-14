use formality_core::{judgment_fn, set, ProvenSet, Set};
use formality_types::grammar::{AtomicEffect, Effect, Relation, Wcs};
use std::fmt::Debug;

use crate::{prove::{prove_after::prove_after, prove_eq::prove_traitref_eq}, Decls};

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
            // first prove that `A -subset- C`
            (prove_effect_subset(&decls, env, &assumptions, &*subset1, &superset) => c)

            // now prove that `B -subset- C`
            (prove_after(&decls, c, &assumptions, Relation::effect_subset(&*subset2, &superset)) => c)
            --- ("union")
            (prove_effect_subset(decls, env, assumptions, Effect::Union(subset1, subset2), superset) => c)
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

        // Example: we are trying to prove `X <= S`
        (
            // If we can find some other effect `B` such that `X <= B`...
            (prove_effect_upper_bound(&decls, env, &assumptions, &atomic_subeffect) => (c, bounding_effect))

            // ...and we can prove `B <= S`, then we know `X <= S` (transitivity).
            (prove_after(&decls, c, &assumptions, Relation::effect_subset(&*bounding_effect, &superset)) => c)
            ----------------------------- ("transitive")
            (prove_atomic_effect_subset(decls, env, assumptions, atomic_subeffect, superset) => c)
        )

    }
}


judgment_fn! {
    /// `prove_effect_bound(..., a) => b` means that `a <= b`
    fn prove_effect_upper_bound(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        f1: AtomicEffect,
    ) => (Constraints, AtomicEffect) {
        debug(f1, assumptions, env)


        (
            --- ("runtime bounded by const")
            (prove_effect_upper_bound(decls, env, assumptions, AtomicEffect::Const) => (Constraints::none(env), AtomicEffect::Runtime))
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
            (some_atomic_effect(&*f1) => e)
            --- ("union-lhs")
            (some_atomic_effect(Effect::Union(f1, _f2)) => e)
        )

        (
            (some_atomic_effect(&*f2) => e)
            --- ("union-rhs")
            (some_atomic_effect(Effect::Union(_f1, f2)) => e)
        )

        (
            --- ("atomic")
            (some_atomic_effect(Effect::Atomic(e)) => e)
        )
    }
}
