use formality_types::{
    cast::{Downcast, Upcast, Upcasted},
    collections::Deduplicate,
    grammar::{
        AliasTy, AtomicRelation, InferenceVar, Parameter, RigidTy, Substitution, Ty, TyData,
        Variable, Wcs,
    },
    judgment_fn,
    visit::Visit,
};

use crate::{
    program::Program,
    prove::{
        constraints::no_constraints, prove, prove_after::prove_after,
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
            (if let None = t.downcast::<InferenceVar>())
            (if let Some(env_c) = equate_variable(env, v, t))
            ----------------------------- ("existential-l")
            (prove_ty_eq(_program, env, _assumptions, Variable::InferenceVar(v), t) => env_c)
        )

        (
            (if let None = t.downcast::<InferenceVar>())
            (if let Some(env_c) = equate_variable(env, v, t))
            ----------------------------- ("existential-r")
            (prove_ty_eq(_program, env, _assumptions, t, Variable::InferenceVar(v)) => env_c)
        )

        (
            // Map the higher rank variable to the lower rank one.
            (let (a, b) = env.order_by_universe(l, r))
            ----------------------------- ("existential-both")
            (prove_ty_eq(_program, env, _assumptions, Variable::InferenceVar(l), Variable::InferenceVar(r)) => (env, (b, a)))
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
    mut env: Env,
    x: InferenceVar,
    p: impl Upcast<Parameter>,
) -> Option<(Env, Constraints)> {
    let p: Parameter = p.upcast();

    let span = tracing::debug_span!("equate_variable", ?x, ?p, ?env);
    let _guard = span.enter();

    let fvs = p.free_variables().deduplicate();

    // Ensure that `x` passes the occurs check for the free variables in `p`.
    if !passes_occurs_check(&env, x, &fvs) {
        return None;
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
        .flat_map(|&fv| {
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
    // * `fv = universe_subst(fv)` for each free variable `fv` in `p` (e.g., `Y => Z` in our example above)
    // * `x = universe_subst(p)` (e.g., `Vec<Z>` in our example above)
    let constraints: Constraints = universe_subst
        .iter()
        .chain(Some((x, universe_subst.apply(&p)).upcast()))
        .collect();

    tracing::debug!("success: env={:?}, constraints={:?}", env, constraints);
    Some((env, constraints))
}

/// An existential variable `x` *passes the occurs check* with respect to
/// a set of free variables `fvs` if
///
/// * `x` is not a member of `fvs`
/// * all placeholders in `fvs` are in a lower universe than `v`
fn passes_occurs_check(env: &Env, x: InferenceVar, fvs: &[Variable]) -> bool {
    env.assert_encloses((x, fvs));

    let universe_x = env.universe(x);
    for fv in fvs {
        match fv {
            Variable::PlaceholderVar(pv) => {
                if universe_x < env.universe(pv) {
                    return false;
                } else {
                }
            }
            Variable::InferenceVar(iv) => {
                if *iv == x {
                    return false;
                } else {
                }
            }
            Variable::BoundVar(_) => {
                panic!("unexpected bound variable");
            }
        }
    }

    true
}
