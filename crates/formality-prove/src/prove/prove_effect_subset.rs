use formality_core::{judgment_fn, ProvenSet, Set, set};
use formality_types::grammar::{Effect, Wcs};
use std::fmt::Debug;

use crate::{prove::prove_eq::prove_traitref_eq, Decls};

use super::{Constraints, Env};


//judgment_fn! {
//    pub fn prove_effect_subset(
//        _decls: Decls,
//        env: Env,
//        assumptions: Wcs,
//        f1: Effect,
//        f2: Effect,
//    ) => Constraints {
//        debug(f1, f2, assumptions, env)
//        trivial(f1 == f2 => Constraints::empty())
//
//        // fn foo<T>() { }
//        // (<!T as Default>) == (<!T as Default>)
//        //
//        // (<?X as Default>) == (<!T as Default>) => [?X => !T]
//
//        (
//            // forall E1 in Subset
//            //     exists E2 in Superset
//            //          E1 == E2
//            
//            
//            --- ("union-subset-lhs")
//            (prove_effect_subset(decls, env, assumptions, f1, Effect::Union(f2_1, f2_2)) => constraints)
//        )
//    }
//}


judgment_fn! {
    fn prove_atomic_effect_subset_effect(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        atomic_subset: Effect,
        superset: Effect,
    ) => Constraints {
        debug(atomic_subset, superset, assumptions, env)

        (
            // find some atomic effect in the superset...
            (some_atomic_effect(superset) => atomic_superset)
            // ...and prove it is equal to the atomic from the subset
            (prove_atomic_effect_eq(&decls, &env, &assumptions, atomic_subset, atomic_superset) => constraints)
            --- ("union-subset-lhs")
            (prove_atomic_effect_subset_effect(decls, env, assumptions, _f1, Effect::Union(_f2_1, _f2_2)) => constraints)
        )
    }
}


judgment_fn! {
    fn prove_atomic_effect_eq(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        f1: Effect,
        f2: Effect,
    ) => Constraints {
        debug(f1, f2, assumptions, env)
        trivial(f1 == f2 => Constraints::none(env))

        (
            (prove_traitref_eq(decls, env, assumptions, &*tr1, &*tr2) => constraints)
            --- ("associated-effect")
            (prove_atomic_effect_eq(decls, env, assumptions, Effect::AssociatedEffect(tr1), Effect::AssociatedEffect(tr2)) => constraints)
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
    fn some_atomic_effect(
        f1: Effect
    ) => Effect {
        debug(f1)

        (
            --- ("union-lhs")
            (some_atomic_effect(Effect::Union(f1, _f2)) => &*f1)
        )

        (
            --- ("union-rhs")
            (some_atomic_effect(Effect::Union(_f1, f2)) => &*f2)
        )

        (
            --- ("assoc-effect")
            (some_atomic_effect(Effect::AssociatedEffect(tr)) => Effect::AssociatedEffect(tr))
        )

        (
            --- ("assoc-const")
            (some_atomic_effect(Effect::Const) => Effect::Const)
        )

        (
            --- ("assoc-runtime")
            (some_atomic_effect(Effect::Runtime) => Effect::Runtime)
        )
    }
}