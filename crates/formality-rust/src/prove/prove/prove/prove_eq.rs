use crate::grammar::{
    AliasTy, ExistentialVar, Parameter, Relation, RigidTy, Substitution, TyData, UniversalVar,
    Variable, Wcs,
};
use crate::prove::prove::Constrained;
use formality_core::judgment::FailureLocation;
use formality_core::visit::CoreVisit;
use formality_core::Deduplicate;
use formality_core::{judgment_fn, Downcast, ProvenSet, Upcast};

use crate::prove::prove::{
    decls::Program,
    prove::{constraints::occurs_in, prove, prove_normalize::prove_normalize},
};

use super::env::Env;

/// Goal(s) to prove `a` and `b` are equal
pub fn eq(a: impl Upcast<Parameter>, b: impl Upcast<Parameter>) -> Relation {
    Relation::equals(a, b)
}

judgment_fn! {
    pub fn prove_eq(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        a: Parameter,
        b: Parameter,
    ) => Env {
        debug(a, b, assumptions, env)

        assert(a.kind() == b.kind())

        trivial(a == b => env)

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
            // We expect that all existential variables have been substituted away.
            (assert !env.substitution().maps(v))
            (prove_existential_var_eq(decls, env, assumptions, v, r) => c)
            ----------------------------- ("existential")
            (prove_eq(decls, env, assumptions, Variable::ExistentialVar(v), r) => c)
        )

        (
            (prove_normalize(decls, env, assumptions, x) => Constrained(y, env))
            (prove(decls, env, assumptions, eq(y, z)) => env)
            ----------------------------- ("normalize-l")
            (prove_eq(decls, env, assumptions, x, z) => env)
        )
    }
}

// TODO(step0): to resolve the error here, complete the two steps in crates/formality-rust/src/prove/prove/prove/env.rs
judgment_fn! {
    pub fn prove_existential_var_eq(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        v: ExistentialVar,
        b: Parameter,
    ) => Env {
        debug(v, b, assumptions, env)

        // If the RHS is *not* a variable, e.g., we are trying to prove something like this
        //
        // * `?A = u32`
        // * `?A = 'static`
        //
        // then we have learned something about what `?A` must be. The
        // `equate_variable` judgment manages that case.
        (
            (if let None = t.downcast::<Variable>())
            (equate_variable(decls, env, assumptions, v, t) => env)
            ----------------------------- ("existential-nonvar")
            (prove_existential_var_eq(decls, env, assumptions, v, t) => env)
        )

        // If the RHS IS an existential variable, e.g., we are trying to prove something like this
        //
        // * `?A = ?B`
        //
        // then we can either map `?A` to `?B` or vice versa.
        // Whichever way, they must be the same.
        //
        // We pick the variable with the higher universe and map it to the one with the lower universe,
        // which makes sense, consider:
        //
        // exists<A> { forall<C> { exists<B> { A = B } } && A = u32 }
        // ---------   ---------   ---------
        // |           |           |
        // |           universe 1  universe 1
        // universe 0
        //
        // B is not in scope everywhere that A is in scope, so we can't replace
        // A with B universally. But we CAN replace B with a universally.
        (
            // Map the higher rank variable to the lower rank one.
            (let (a, b) = env.order_by_universe(l, r))
            ----------------------------- ("existential-existential")
            (prove_existential_var_eq(_decls, env, _assumptions, l, Variable::ExistentialVar(r)) => ((b, a), env))
        )

        // If the RHS IS a universal variable, e.g., we are trying to prove something like this
        //
        // * `?A = !B`
        //
        // then we can map `?A` to `!B`, so long as the universes work out:
        //
        // exists<A> { exists<B> { A = B } && A = u32 }
        // ---------   ---------
        // |           |
        // |           universe 2
        // universe 1
        //
        // B is not in scope everywhere that A is in scope, so we can't replace
        // A with B universally. But we CAN replace B with a universally.
        (
            (if env.universe(p) < env.universe(v))
            ----------------------------- ("existential-universal")
            (prove_existential_var_eq(_decls, env, _assumptions, v, Variable::UniversalVar(p)) =>  ((v, p), env))
        )
    }
}

#[track_caller]
fn equate_variable(
    decls: impl Upcast<Program>,
    env: impl Upcast<Env>,
    assumptions: impl Upcast<Wcs>,
    x: impl Upcast<ExistentialVar>,
    p: impl Upcast<Parameter>,
) -> ProvenSet<Env> {
    let decls: Program = decls.upcast();
    let mut env: Env = env.upcast();
    let assumptions: Wcs = assumptions.upcast();
    let x: ExistentialVar = x.upcast();
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
        return ProvenSet::failed(
            "equate_variable",
            FailureLocation::caller(),
            format!("`{x:?}` occurs in `{p:?}`"),
        );
    }

    // forall<A> {
    //   exists<B> {
    //     A = B // provable
    //   }
    // }

    // exists<A> {
    //   forall<B> {
    //     A = B // not provable
    //   }
    // }

    // exists<A> { // U0
    //   forall<B> { // U1
    //     exists<C> { // U2
    //       A = Vec<C>
    //       C = B // not provable
    //       // Together: A = Vec<B> // and an existential variable in U0 cannot name a name in U1
    //     }
    //   }
    // }
    //
    // substitution for a variable in Ux can only mention variables in Ux or below

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

    // [A, B, C] |- A = Vec<C>
    // [Z, A, B, C] |- (A = Vec<Z>) && (Z = C)
    // [Z, A, B, C] |- (A = <A as Trait<C>>::Foo) // where forall<C> <A as Trait<C>>::Foo = A
    // [Z, A, B, C] |- (A = <A as Trait<Z>>::Foo) && (Z = C)
    // [Z, A, B, C] |- (A = A) && (Z = C)
    // we should explore this

    // Introduce the following constraints:
    //
    // * `fv = universe_subst(fv)` for each free existential variable `fv` in `p` (e.g., `Y => Z` in our example above)
    // * `x = universe_subst(p)` (e.g., `Vec<Z>` in our example above)
    let new_subst = universe_subst
        .iter()
        .filter(|(v, _)| v.is_a::<ExistentialVar>())
        .chain(Some((x, universe_subst.apply(&p)).upcast()))
        .collect();

    let env = env.update_substitution(new_subst);

    // For each universal variable that we replaced with an existential variable
    // above, we now have to prove that goal. e.g., if we had `X = Vec<!Y>`, we would replace `!Y` with `?Z`
    // (where `?Z` is in a lower universe than `X`), but now we must prove that `!Y = ?Z`
    // (this may be possible due to assumptions).
    let goals: Wcs = universe_subst
        .iter()
        .filter(|(v, _)| v.is_a::<UniversalVar>())
        .map(|(v, p)| eq(v, p))
        .collect();

    tracing::debug!("equated: env={:?}, goals={:?}", env, goals);

    prove(decls, env, assumptions, goals)
}
