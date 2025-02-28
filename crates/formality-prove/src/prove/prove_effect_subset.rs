use formality_core::visit::CoreVisit;
use formality_core::{judgment_fn, Downcast, ProvenSet, Upcast};
use formality_core::{Deduplicate, Upcasted};
use formality_types::grammar::{
    AliasTy, ExistentialVar, Parameter, Relation, RigidTy, Substitution, TyData, UniversalVar,
    Variable, Wcs,
};

use crate::{
    decls::Decls,
    prove::{
        constraints::occurs_in, prove, prove_after::prove_after, prove_normalize::prove_normalize,
    },
};

use super::{constraints::Constraints, env::Env};

/// Goal(s) to prove `a` and `b` are equal
pub fn eq(a: impl Upcast<Parameter>, b: impl Upcast<Parameter>) -> Relation {
    Relation::equals(a, b)
}

judgment_fn! {
    pub fn prove_effect_subset(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        f1: Effect
        f2: Effect,
    ) => Constraints {
        debug(f1, f2, assumptions, env)
        trivial(f1 == f2 => Constraints::empty())

        // fn foo<T>() { }
        // (<!T as Default>) == (<!T as Default>)
        //
        // (<?X as Default>) == (<!T as Default>) => [?X => !T]

        (
            // forall E1 in Subset
            //     exists E2 in Superset
            //          E1 == E2
            
            
            --- ("union-subset-lhs")
            (prove_effect_subset(decls, env, assumptions, f1, Effect::Union(f2_1, f2_2)) => constraints)
        )
    }
}


judgment_fn! {
    fn prove_atomic_effect_subset_effect(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        atomic_subset: Effect
        superset: Effect,
    ) => Constraints {
        debug(atomic, superset, assumptions, env)

        (
            // find some atomic effect in the superset...
            (some_atomic_effect(superset) => atomic_superset)

            // ...and prove it is equal to the atomic from the subset
            (prove_atomic_effect_eq(decls, env, assumptions, atomic_subset, atomic_superset) => constraints)
            --- ("union-subset-lhs")
            (prove_atomic_effect_subset_effect(decls, env, assumptions, f1, Effect::Union(f2_1, f2_2)) => constraints)
        )
    }
}


judgment_fn! {
    fn prove_atomic_effect_eq(
    ) => Constraints {
        debug(f1, f2, assumptions, env)
        trivial(f1 == f2 => Constraints::empty())

        (
            (prove_trait_ref_eq(decls, env, assumptions, tr1, tr2) => constraints)
            --- ("associated-effect")
            (prove_effect_subset(decls, env, assumptions, Effect::AssociatedEffect(tr1), Effect::AssociatedEffect(tr2)) => constraints)
        )
    }
)

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
            (atomic_effect(Effect::Union(f1, _f2)) => f1)
        )

        (
            --- ("union-rhs")
            (atomic_effect(Effect::Union(_f1, f2)) => f2)
        )

        (
            --- ("assoc-effect")
            (atomic_effect(Effect::AssociatedEffect(tr)) => Effect::AssociatedEffect(tr))
        )

        (
            --- ("assoc-const")
            (atomic_effect(Effect::Const) => Effect::Const)
        )

        (
            --- ("assoc-runtime")
            (atomic_effect(Effect::Runtime) => Effect::Runtime)
        )
    }
}