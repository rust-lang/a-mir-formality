use formality_core::{judgment_fn, set, Set};
use formality_types::grammar::{
    Lt, Parameter, RigidName, RigidTy, TraitRef, TyData, Variable, Wcs,
};

use crate::{
    decls::Decls,
    prove::{combinators::for_all, prove_normalize::prove_normalize, Constraints},
    Env,
};

// From https://rust-lang.github.io/rfcs/2451-re-rebalancing-coherence.html:
//
// Given `impl<P1..=Pn> Trait<T1..=Tn> for T0`, an impl is valid only if at least one of the following is true:
//
// - `Trait` is a local trait
// - All of
//  - At least one of the types `T0..=Tn` must be a local type. Let `Ti` be the
//    first such type.
//  - No uncovered type parameters `P1..=Pn` may appear in `T0..Ti` (excluding
//    `Ti`)
//
// Given the following definitions:
//
// Covered Type: A type which appears as a parameter to another type. For example,
// `T` is uncovered, but the `T` in `Vec<T>` is covered. This is only relevant for
// type parameters.
//
// Fundamental Type: A type for which you cannot add a blanket impl backwards
// compatibly. This includes `&`, `&mut`, and `Box`. Any time a type `T` is
// considered local, `&T`, `&mut T`, and `Box<T>` are also considered local.
// Fundamental types cannot cover other types. Any time the term "covered type" is
// used, `&T`, `&mut T`, and `Box<T>` are not considered covered.
//
// Local Type: A struct, enum, or union which was defined in the current crate.
// This is not affected by type parameters. `struct Foo` is considered local, but
// `Vec<Foo>` is not. `LocalType<ForeignType>` is local. Type aliases and trait
// aliases do not affect locality.

/// True if `goal` may be remote. This is
pub fn may_be_remote(decls: Decls, env: Env, assumptions: Wcs, goal: TraitRef) -> Set<Constraints> {
    assert!(env.is_in_coherence_mode());

    let c = is_local_trait_ref(decls, &env, assumptions, goal);

    if !c.is_proven() {
        // Cannot possibly be local, so always remote.
        return set![Constraints::none(env)];
    }

    if c.iter().any(Constraints::unconditionally_true) {
        // If this is unconditionally known to be local, then it is never remote.
        return set![];
    }

    // Otherwise it is ambiguous
    set![Constraints::none(env).ambiguous()]
}

judgment_fn! {
    pub fn is_local_trait_ref(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        goal: TraitRef,
    ) => Constraints {
        debug(goal, assumptions, env, decls)

        (
            (if decls.is_local_trait_id(&goal.trait_id))
            --- ("local trait")
            (is_local_trait_ref(decls, env, _assumptions, goal) => Constraints::none(env))
        )

        (
            (0 .. goal.parameters.len() => i)
            (is_local_parameter(&decls, &env, &assumptions, &goal.parameters[i]) => c1)
            (let assumptions = c1.substitution().apply(&assumptions))
            (let goal = c1.substitution().apply(&goal))
            (for_all(&decls, &env, &assumptions, &goal.parameters[..i], &not_downstream) => c2)
            --- ("local parameter")
            (is_local_trait_ref(decls, env, assumptions, goal) => c1.seq(c2))
        )
    }
}

judgment_fn! {
    /// "not_downstream(..., P)" means that `P` cannot be instantiated with a type from
    /// a downstream crate (i.e., a crate that has us as a dependency).
    ///
    /// NB. Since RFC 2451, the judgment applies to the outermost type only. In other words,
    /// the judgment holds for (e.g.) `Vec<T>`, which could be instantiated
    /// with something like `Vec<DownstreamType>`, but that is not considered downstream
    /// as the outermost type (`Vec`) is upstream.
    fn not_downstream(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        parameter: Parameter,
    ) => Constraints {
        debug(parameter, assumptions, env, decls)

        (
            // Since https://rust-lang.github.io/rfcs/2451-re-rebalancing-coherence.html,
            // any rigid type is adequate.
            --- ("rigid")
            (not_downstream(_decls, env, _assumptions, RigidTy { .. }) => Constraints::none(env))
        )

        (
            // Lifetimes are not relevant.
            --- ("lifetime")
            (not_downstream(_decls, env, _assumptions, _l: Lt) => Constraints::none(env))
        )

        (
            // existential variables *could* be inferred to downstream types; depends on the substitution
            // we ultimately have.
            --- ("type variable")
            (not_downstream(_decls, env, _assumptions, TyData::Variable(Variable::ExistentialVar(_))) => Constraints::none(env).ambiguous())
        )
    }
}

judgment_fn! {
    fn is_local_parameter(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        goal: Parameter,
    ) => Constraints {
        debug(goal, assumptions, env, decls)

        assert(env.is_in_coherence_mode())

        // If we can normalize `goal` to something else, check if that normalized form is local.
        (
            (prove_normalize(&decls, env.with_coherence_mode(false), &assumptions, goal) => (c1, p))
            (let c1 = c1.with_coherence_mode(true))
            (let assumptions = c1.substitution().apply(&assumptions))
            (is_local_parameter(&decls, c1.env(), assumptions, p) => c2)
            --- ("local parameter")
            (is_local_parameter(decls, env, assumptions, goal) => c1.seq(c2))
        )

        // Fundamental types are local if all their arguments are local.
        (
            (if is_fundamental(&decls, &name))
            (for_all(&decls, &env, &assumptions, &parameters, &is_local_parameter) => c) // FIXME: should be `is_local_parameter`
            --- ("fundamental rigid type")
            (is_local_parameter(decls, env, assumptions, RigidTy { name, parameters }) => c)
        )

        // ADTs are local if they were declared in this crate.
        (
            (if decls.is_local_adt_id(&a))
            --- ("local rigid type")
            (is_local_parameter(decls, env, _assumptions, RigidTy { name: RigidName::AdtId(a), parameters: _ }) => Constraints::none(env))
        )

        // existential variables might or might not be local, depending on how they are instantiated.
        (
            --- ("existential variable")
            (is_local_parameter(_decls, env, _assumptions, TyData::Variable(Variable::ExistentialVar(_))) => Constraints::none(env).ambiguous())
        )
    }
}

fn is_fundamental(_decls: &Decls, name: &RigidName) -> bool {
    // From https://rust-lang.github.io/rfcs/2451-re-rebalancing-coherence.html:
    //
    // Fundamental Type: A type for which you cannot add a blanket impl backwards
    // compatibly. This includes `&`, `&mut`, and `Box`. Any time a type `T` is
    // considered local, `&T`, `&mut T`, and `Box<T>` are also considered local.
    // Fundamental types cannot cover other types. Any time the term "covered type" is
    // used, `&T`, `&mut T`, and `Box<T>` are not considered covered.

    match name {
        RigidName::AdtId(_) => false, // FIXME

        RigidName::Ref(_) => true,

        RigidName::ScalarId(_)
        | RigidName::Tuple(_)
        | RigidName::FnPtr(_)
        | RigidName::FnDef(_) => false,
    }
}
