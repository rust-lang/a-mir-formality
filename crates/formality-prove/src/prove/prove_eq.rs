use formality_types::{
    cast::{Downcast, Upcast, Upcasted},
    collections::{Deduplicate, Set},
    grammar::{
        AliasTy, AtomicRelation, InferenceVar, Parameter, PlaceholderVar, RigidTy, Substitution,
        Ty, TyData, Variable, Wcs,
    },
    judgment_fn, set,
    visit::Visit,
};

use crate::{
    program::Program,
    prove::{
        constraints::{no_constraints, occurs_in},
        prove,
        prove_after::prove_after,
        prove_normalize::prove_normalize,
    },
};

use super::{constraints::Constraints, env::Env};

/// Goal(s) to prove `a` and `b` are equal (they must have equal length)
pub fn all_eq(a: impl Upcast<Vec<Parameter>>, b: impl Upcast<Vec<Parameter>>) -> Wcs {
    let a: Vec<Parameter> = a.upcast();
    let b: Vec<Parameter> = b.upcast();
    assert_eq!(a.len(), b.len());
    a.into_iter()
        .zip(b)
        .map(|(a, b)| AtomicRelation::eq(a, b))
        .upcasted()
        .collect()
}

/// Goal(s) to prove `a` and `b` are equal
pub fn eq(a: impl Upcast<Parameter>, b: impl Upcast<Parameter>) -> AtomicRelation {
    AtomicRelation::eq(a, b)
}

judgment_fn! {
    pub fn prove_ty_eq(
        program: Program,
        env: Env,
        assumptions: Wcs,
        a: Ty,
        b: Ty,
    ) => (Env, Constraints) {
        (
            (if l == r)
            ----------------------------- ("reflexive")
            (prove_ty_eq(_program, env, _assumptions, l, r) => (env, no_constraints()))
        )

        (
            (prove_ty_eq(program, env, assumptions, r, l) => env_c)
            ----------------------------- ("symmetric")
            (prove_ty_eq(program, env, assumptions, l, r) => env_c)
        )

        (
            (let RigidTy { name: a_name, parameters: a_parameters } = a)
            (let RigidTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)
            (prove(program, env, assumptions, all_eq(a_parameters, b_parameters)) => c)
            ----------------------------- ("rigid")
            (prove_ty_eq(program, env, assumptions, TyData::RigidTy(a), TyData::RigidTy(b)) => c)
        )

        (
            (let AliasTy { name: a_name, parameters: a_parameters } = a)
            (let AliasTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)
            (prove(program, env, assumptions, all_eq(a_parameters, b_parameters)) => env_c)
            ----------------------------- ("alias")
            (prove_ty_eq(program, env, assumptions, TyData::AliasTy(a), TyData::AliasTy(b)) => env_c)
        )

        (
            (if let None = t.downcast::<Variable>())
            (equate_variable(program, env, assumptions, v, t) => (env, c))
            ----------------------------- ("existential-nonvar")
            (prove_ty_eq(program, env, assumptions, Variable::InferenceVar(v), t) => (env, c))
        )

        (
            // Map the higher rank variable to the lower rank one.
            (let (a, b) = env.order_by_universe(l, r))
            ----------------------------- ("existential-existential")
            (prove_ty_eq(_program, env, _assumptions, Variable::InferenceVar(l), Variable::InferenceVar(r)) => (env, (b, a)))
        )

        (
            (if env.universe(p) < env.universe(v))
            ----------------------------- ("existential-placeholder")
            (prove_ty_eq(_program, env, _assumptions, Variable::InferenceVar(v), Variable::PlaceholderVar(p)) => (env, (v, p)))
        )

        (
            (prove_normalize(&program, env, &assumptions, &x) => (env1, y, c1))
            (prove_after(&program, env1, c1, &assumptions, eq(y, &z)) => (env2, c2))
            ----------------------------- ("normalize-l")
            (prove_ty_eq(program, env, assumptions, x, z) => (env2, c2))
        )
    }
}

fn equate_variable(
    program: Program,
    mut env: Env,
    assumptions: Wcs,
    x: InferenceVar,
    p: impl Upcast<Parameter>,
) -> Set<(Env, Constraints)> {
    let p: Parameter = p.upcast();

    let span = tracing::debug_span!("equate_variable", ?x, ?p, ?env);
    let _guard = span.enter();

    // Preconditions:
    // * Environment contains all free variables
    // * `p` is some compound type, not a variable
    //   (variables are handled via special rules above)
    env.assert_encloses((x, (&assumptions, &p)));
    assert!(!p.is_a::<Variable>());

    let fvs = p.free_variables().deduplicate();

    // Ensure that `x` passes the occurs check for the free variables in `p`.
    if occurs_in(x, &fvs) {
        return set![];
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
    let constraints: Constraints = universe_subst
        .iter()
        .filter(|(v, _)| v.is_a::<InferenceVar>())
        .chain(Some((x, universe_subst.apply(&p)).upcast()))
        .collect();

    // For each placeholder variable that we replaced with an inference variable
    // above, we now have to prove that goal. e.g., if we had `X = Vec<!Y>`, we would replace `!Y` with `?Z`
    // (where `?Z` is in a lower universe than `X`), but now we must prove that `!Y = ?Z`
    // (this may be posible due to assumptions).
    let goals: Wcs = universe_subst
        .iter()
        .filter(|(v, _)| v.is_a::<PlaceholderVar>())
        .map(|(v, p)| eq(v, p))
        .upcasted()
        .collect();

    tracing::debug!(
        "equated: env={:?}, constraints={:?}, goals={:?}",
        env,
        constraints,
        goals
    );

    prove_after(program, env, constraints, assumptions, goals)
}
