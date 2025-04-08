use formality_core::visit::CoreVisit;
use formality_core::{judgment_fn, Downcast, ProvenSet, Upcast, UpcastFrom};
use formality_core::{Deduplicate, Upcasted};
use formality_types::grammar::TraitRef;
use formality_types::grammar::WcData;
use formality_types::grammar::{
    AliasTy, ExistentialVar, Parameter, Relation, RigidTy, Substitution, TyData, UniversalVar,
    Variable, Wc, Wcs,
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
    pub fn prove_eq(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        a: Parameter,
        b: Parameter,
    ) => Constraints {
        debug(a, b, assumptions, env)

        assert(a.kind() == b.kind())

        trivial(a == b => Constraints::none(env))

        (
            (prove_eq(decls, env, assumptions, r, l) => env_c)
            ----------------------------- ("symmetric")
            (prove_eq(decls, env, assumptions, l, r) => env_c)
        )

        (
            (let RigidTy { name: a_name, parameters: a_parameters } = a)
            (let RigidTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)!
            (prove(decls, env, assumptions, Wcs::all_eq(a_parameters, b_parameters)) => c)
            ----------------------------- ("rigid")
            (prove_eq(decls, env, assumptions, TyData::RigidTy(a), TyData::RigidTy(b)) => c)
        )

        (
            (let AliasTy { name: a_name, parameters: a_parameters } = a)
            (let AliasTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)!
            (prove(decls, env, assumptions, Wcs::all_eq(a_parameters, b_parameters)) => env_c)
            ----------------------------- ("alias")
            (prove_eq(decls, env, assumptions, TyData::AliasTy(a), TyData::AliasTy(b)) => env_c)
        )

        (
            (prove_existential_var_eq(decls, env, assumptions, v, r) => c)
            ----------------------------- ("existential")
            (prove_eq(decls, env, assumptions, Variable::ExistentialVar(v), r) => c)
        )

        // Example: we are trying to prove `x` (which equals `<SomeX<?V> as Iterator>::Item`)
        // is equal to some type `z`.
        (
            // Normalize `x` will find alternative "spellings" that it is equivalent to.
            // For example, if there is an impl like
            // `impl Iterator for SomeX<i32> { type Item = u32; ... }`
            // then `prove_normalize` would yield `(c, u32)` where `c` are any constraints
            // needed to show that it normalized (in this case, `c` would include the
            // substitution `?V = i32`).
            (prove_normalize(&decls, env, &assumptions, &x) => (c, y))

            // Now that we know that `x` is equivalent to `y`, we try to prove
            // that `y` is equivalent to `z` (our original goal).
            // We do that with `prove_after` so that the constraints `c` are considered
            // (e.g., if `z` includes `?V`, it will be replaced with `i32`).
            (prove_after(&decls, c, &assumptions, eq(y, &z)) => c)
            ----------------------------- ("normalize-l")
            (prove_eq(decls, env, assumptions, x, z) => c)
        )
    }
}

judgment_fn! {
    pub fn prove_traitref_eq(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        a: TraitRef,
        b: TraitRef,
    ) => Constraints {
        debug(a, b, assumptions, env)

        trivial(a == b => Constraints::none(env))

        // Two trait_refs are equal if they have the same id, effect, and parameters.
        (
            (let TraitRef { effect: a_effect, trait_id: a_trait_id, parameters: a_parameters } = a)
            (let TraitRef { effect: b_effect, trait_id: b_trait_id, parameters: b_parameters } = b)
            (if a_trait_id == b_trait_id)!
            (prove(&decls, env, assumptions.clone(), Wcs::all_eq(a_parameters, b_parameters)) => constraints)
            (prove_after(&decls, constraints, assumptions.clone(), Wc::upcast_from(WcData::EffectSubset(a_effect.clone(), b_effect.clone()))) => constraints)
            (prove_after(&decls, constraints, assumptions.clone(),Wc::upcast_from(WcData::EffectSubset(b_effect.clone(), a_effect.clone()))) => constraints)
            ----------------------------- ("traitref")
            (prove_traitref_eq(decls, env, assumptions, a, b) => constraints)
        )
    }
}

judgment_fn! {
    pub fn prove_existential_var_eq(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        v: ExistentialVar,
        b: Parameter,
    ) => Constraints {
        debug(v, b, assumptions, env)

        (
            (if let None = t.downcast::<Variable>())
            (equate_variable(decls, env, assumptions, v, t) => c)
            ----------------------------- ("existential-nonvar")
            (prove_existential_var_eq(decls, env, assumptions, v, t) => c)
        )

        (
            // Map the higher rank variable to the lower rank one.
            (let (a, b) = env.order_by_universe(l, r))
            ----------------------------- ("existential-existential")
            (prove_existential_var_eq(_decls, env, _assumptions, l, Variable::ExistentialVar(r)) => (env, (b, a)))
        )

        (
            (if env.universe(p) < env.universe(v))
            ----------------------------- ("existential-universal")
            (prove_existential_var_eq(_decls, env, _assumptions, v, Variable::UniversalVar(p)) => (env, (v, p)))
        )
    }
}

fn equate_variable(
    decls: Decls,
    mut env: Env,
    assumptions: Wcs,
    x: ExistentialVar,
    p: impl Upcast<Parameter>,
) -> ProvenSet<Constraints> {
    let p: Parameter = p.upcast();

    let span = tracing::debug_span!("equate_variable", ?x, ?p, ?env);
    let _guard = span.enter();

    // Preconditions:
    // * Environment contains all free variables
    // * `p` is some compound type, not a variable
    //   (variables are handled via special rules above)
    assert!(env.encloses((x, (&assumptions, &p))));
    assert!(!p.is_a::<Variable>());

    let fvs = p.free_variables().deduplicate();

    // Ensure that `x` passes the occurs check for the free variables in `p`.
    if occurs_in(x, &fvs) {
        return ProvenSet::failed("equate_variable", format!("`{x:?}` occurs in `{p:?}`"));
    }

    // Map each free variable `fv` in `p` that is of higher universe than `x`
    // to a fresh variable `y` of lower universe than `x`.
    //
    // e.g., in an environment `[X, Y]`, if we have `X = Vec<Y>`:
    // * we would create `Z` before `X` (so new env is `[Z, X, Y]`)
    // * and map `Y` to `Z`
    let universe_x = env.universe(x);
    let universe_subst: Substitution = fvs
        .iter()
        .flat_map(|fv| {
            if universe_x < env.universe(fv) {
                let y = env.insert_fresh_before(fv.kind(), universe_x);
                Some((fv, y))
            } else {
                None
            }
        })
        .collect();

    // Introduce the following constraints:
    //
    // * `fv = universe_subst(fv)` for each free existential variable `fv` in `p` (e.g., `Y => Z` in our example above)
    // * `x = universe_subst(p)` (e.g., `Vec<Z>` in our example above)
    let constraints: Constraints = Constraints::from(
        env,
        universe_subst
            .iter()
            .filter(|(v, _)| v.is_a::<ExistentialVar>())
            .chain(Some((x, universe_subst.apply(&p)).upcast())),
    );

    // For each universal variable that we replaced with an existential variable
    // above, we now have to prove that goal. e.g., if we had `X = Vec<!Y>`, we would replace `!Y` with `?Z`
    // (where `?Z` is in a lower universe than `X`), but now we must prove that `!Y = ?Z`
    // (this may be possible due to assumptions).
    let goals: Wcs = universe_subst
        .iter()
        .filter(|(v, _)| v.is_a::<UniversalVar>())
        .map(|(v, p)| eq(v, p))
        .upcasted()
        .collect();

    tracing::debug!("equated: constraints={:?}, goals={:?}", constraints, goals);

    prove_after(decls, constraints, assumptions, goals)
}
