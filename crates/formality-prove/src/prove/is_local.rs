use formality_core::{judgment_fn, ProvenSet, Upcast};
use formality_types::grammar::{
    AliasTy, BoundVar, Lt, Parameter, RigidName, RigidTy, TraitRef, TyData, Variable, Wcs,
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

judgment_fn! {
    /// True if `goal` may be implemented in a crate that is not the current one.
    /// This could be a downstream crate that we cannot see, or it could be a future
    /// (semver-compatible) version of an upstream crate.
    ///
    /// Note that per RFC #2451, upstream crates are not permitted to add blanket impls
    /// and so new upstream impls for local types cannot be added.
    pub fn may_be_remote(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        goal: TraitRef,
    ) => () {
        debug(assumptions, goal, decls, env)
        assert(env.is_in_coherence_mode())

        (
            (may_be_downstream_trait_ref(decls, env, assumptions, goal) => ())
            --- ("may be defined downstream")
            (may_be_remote(decls, env, assumptions, goal) => ())
        )

        (
            // In principle this rule could be removed and preserve soundness,
            // but then we would accept code that is very prone to semver failures.
            (may_not_be_provable(&env, |env| is_local_trait_ref(decls, &env, assumptions, goal)) => ())
            --- ("may be added by upstream in a minor release")
            (may_be_remote(decls, env, assumptions, goal) => ())
        )
    }
}

judgment_fn! {
    /// True if an impl defining this trait-reference could appear in a downstream crate.
    fn may_be_downstream_trait_ref(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        goal: TraitRef,
    ) => () {
        debug(goal, assumptions, env, decls)
        assert(env.is_in_coherence_mode())
        (
            // There may be a downstream parameter at position i...
            (&goal.parameters => p)
            (may_be_downstream_parameter(&decls, &env, &assumptions, p) => ())
            --- ("may_be_downstream_trait_ref")
            (may_be_downstream_trait_ref(decls, env, assumptions, goal) => ())
        )
    }
}

judgment_fn! {
    fn may_be_downstream_parameter(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        parameter: Parameter,
    ) => () {
        debug(parameter, assumptions, env, decls)
        assert(env.is_in_coherence_mode())
        (
            // existential variables *could* be inferred to downstream types; depends on the substitution
            // we ultimately have.
            --- ("type variable")
            (may_be_downstream_parameter(_decls, _env, _assumptions, TyData::Variable(Variable::ExistentialVar(_))) => ())
        )

        // If `parameter` is an alias which refers a type which may be
        // from a downstream crate, it may be normalized to that type,
        // so recurse into it.
        //
        // We only do so if the alias cannot be normalized to a type which
        // is definitely not a downstream parameter.
        (
            // (a) there is some parameter in the alias that may be downstream
            (parameters.iter() => p)
            (if may_contain_downstream_type(&decls, &env, &assumptions, p))

            // (b) the alias cannot be normalized to something that may not be downstream
            (may_not_be_provable(&env, |env|
                normalizes_to_not_downstream(&decls, &env, &assumptions, AliasTy { name: name.clone(), parameters: parameters.clone() })
            ) => ())
            --- ("via normalize")
            (may_be_downstream_parameter(decls, env, assumptions, AliasTy { name, parameters }) => ())
        )
    }
}

fn may_contain_downstream_type(
    decls: &Decls,
    env: &Env,
    assumptions: &Wcs,
    parameter: impl Upcast<Parameter>,
) -> bool {
    assert!(env.is_in_coherence_mode());
    let parameter = parameter.upcast();

    let Parameter::Ty(ty) = parameter else {
        return false;
    };

    match ty.data() {
        TyData::RigidTy(RigidTy {
            name: _,
            parameters,
        }) => parameters
            .iter()
            .any(|p| may_contain_downstream_type(decls, env, assumptions, p)),
        TyData::AliasTy(_) => prove_normalize(decls, env, assumptions, ty)
            .iter()
            .any(|(c, p)| {
                let assumptions = c.substitution().apply(assumptions);
                may_contain_downstream_type(decls, env, &assumptions, p)
            }),
        TyData::PredicateTy(p) => match p {
            formality_types::grammar::PredicateTy::ForAll(binder) => {
                let (_, ty) = binder.open();
                may_contain_downstream_type(decls, env, assumptions, ty)
            }
        },
        TyData::Variable(v) => match v {
            Variable::ExistentialVar(_) => true,
            Variable::UniversalVar(_) => panic!("universals are unexpected"),
            Variable::BoundVar(BoundVar {
                debruijn,
                var_index: _,
                kind: _,
            }) => {
                assert!(debruijn.is_none(), "must have been opened on the way down");
                true
            }
        },
    }
}

// FIXME(@lcnr): This should be a more general concept and not limited ot `is_local`
//
// Also, I think this should flip quantification from existential to universal again
fn may_not_be_provable(env: &Env, op: impl FnOnce(Env) -> ProvenSet<Constraints>) -> ProvenSet<()> {
    assert!(env.is_in_coherence_mode());
    if let Some(constraints) = op(env.with_coherence_mode(false))
        .iter()
        .find(|constraints| constraints.unconditionally_true())
    {
        ProvenSet::failed(
            "may_not_be_provable",
            format!("found a solution {constraints:?}"),
        )
    } else {
        ProvenSet::singleton(())
    }
}

judgment_fn! {
    fn normalizes_to_not_downstream(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        parameter: Parameter,
    ) => Constraints {
        debug(parameter, assumptions, env, decls)

        (
            (prove_normalize(&decls, &env, &assumptions, parameter) => (c1, parameter))
            (let assumptions = c1.substitution().apply(&assumptions))
            (is_not_downstream(&decls, &env, assumptions, parameter) => c2)
            --- ("ambiguous")
            (normalizes_to_not_downstream(decls, env, assumptions, parameter) => c1.seq(c2))
        )
    }
}

judgment_fn! {
    pub fn is_local_trait_ref(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        goal: TraitRef,
    ) => Constraints {
        debug(goal, assumptions, env, decls)
        // assert(!env.is_in_coherence_mode()) TODO
        (
            (if decls.is_local_trait_id(&goal.trait_id))
            --- ("local trait")
            (is_local_trait_ref(decls, env, _assumptions, goal) => Constraints::none(env))
        )

        (
            // There is a local parameter at position i...
            (0 .. goal.parameters.len() => i)
            (is_local_parameter(&decls, &env, &assumptions, &goal.parameters[i]) => c1)

            // ...and in positions 0..i, there are no downstream parameters.
            (let assumptions = c1.substitution().apply(&assumptions))
            (let goal = c1.substitution().apply(&goal))
            (for_all(&decls, &env, &assumptions, &goal.parameters[..i], &is_not_downstream) => c2)
            --- ("local parameter")
            (is_local_trait_ref(decls, env, assumptions, goal) => c1.seq(c2))
        )
    }
}

judgment_fn! {
    /// `is_not_downstream(..., P)` means that `P` cannot be instantiated with a type from
    /// a downstream crate (i.e., a crate that has us as a dependency).
    ///
    /// NB. Since RFC 2451, the judgment applies to the outermost type only. In other words,
    /// the judgment holds for (e.g.) `Vec<T>`, which could be instantiated
    /// with something like `Vec<DownstreamType>`, but that is not considered downstream
    /// as the outermost type (`Vec`) is upstream.
    fn is_not_downstream(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        parameter: Parameter,
    ) => Constraints {
        debug(parameter, assumptions, env, decls)
        // assert(!env.is_in_coherence_mode()) // TODO

        (
            // Lifetimes are not relevant.
            --- ("lifetime")
            (is_not_downstream(_decls, env, _assumptions, _l: Lt) => Constraints::none(env))
        )

        (
            // Since https://rust-lang.github.io/rfcs/2451-re-rebalancing-coherence.html,
            // any rigid type is adequate.
            --- ("rigid")
            (is_not_downstream(_decls, env, _assumptions, RigidTy { .. }) => Constraints::none(env))
        )

        (
            (prove_normalize(&decls, env, &assumptions, parameter) => (c1, p))
            (let assumptions = c1.substitution().apply(&assumptions))
            (is_not_downstream(&decls, c1.env(), assumptions, p) => c2)
            --- ("via normalize")
            (is_not_downstream(decls, env, assumptions, parameter) => c1.seq(c2))
        )

        (
            // existential variables *could* be inferred to downstream types; depends on the substitution
            // we ultimately have.
            --- ("type variable")
            (is_not_downstream(_decls, env, _assumptions, TyData::Variable(Variable::ExistentialVar(_))) => Constraints::none(env).ambiguous())
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
        // assert(!env.is_in_coherence_mode()) TODO

        // If we can normalize `goal` to something else, check if that normalized form is local.
        (
            (prove_normalize(&decls, env, &assumptions, goal) => (c1, p))
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
