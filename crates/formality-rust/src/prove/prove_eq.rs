use crate::grammar::{
    AliasTy, ExistentialVar, Parameter, Predicate, RigidTy, Substitution, Ty, UniversalVar,
    Variable, Wcs,
};
use crate::prove::Constrained;
use formality_core::judgment::{FailureLocation, ProofTree};
use formality_core::visit::CoreVisit;
use formality_core::Deduplicate;
use formality_core::{judgment_fn, Downcast, Map, ProvenSet, Set, Upcast};
use std::collections::VecDeque;

use crate::prove::{
    constraints::occurs_in,
    decls::Program,
    prove,
    prove_after::prove_after,
    prove_normalize::{prove_normalize, prove_syntactically_eq},
};

use super::{constraints::Constraints, env::Env};

/// Goal(s) to prove `a` and `b` are equal
pub fn eq(a: impl Upcast<Parameter>, b: impl Upcast<Parameter>) -> Predicate {
    Predicate::equals(a, b)
}

judgment_fn! {
    pub fn prove_eq(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        a: Parameter,
        b: Parameter,
    ) => Constraints {
        debug(a, b, assumptions, env)

        assert(a.kind() == b.kind())

        trivial(a == b => Constraints::none(env))

        (
            (let RigidTy { name: a_name, parameters: a_parameters } = a)
            (let RigidTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)!
            (prove(decls, env, assumptions, Wcs::all_eq(a_parameters, b_parameters)) => c)
            ----------------------------- ("rigid")
            (prove_eq(decls, env, assumptions, Ty::RigidTy(a), Ty::RigidTy(b)) => c)
        )

        (
            (let AliasTy { name: a_name, parameters: a_parameters } = a)
            (let AliasTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)!
            (prove(decls, env, assumptions, Wcs::all_eq(a_parameters, b_parameters)) => env_c)
            ----------------------------- ("alias")
            (prove_eq(decls, env, assumptions, Ty::AliasTy(a), Ty::AliasTy(b)) => env_c)
        )

        (
            (prove_existential_var_eq(decls, env, assumptions, v, r) => c)
            ----------------------------- ("existential")
            (prove_eq(decls, env, assumptions, Variable::ExistentialVar(v), r) => c)
        )

        (
            (prove_existential_var_eq(decls, env, assumptions, v, l) => c)
            ----------------------------- ("existential-r")
            (prove_eq(decls, env, assumptions, l, Variable::ExistentialVar(v)) => c)
        )

        (
            (prove_eq_normalized(decls, env, assumptions, x, z) => c)
            ----------------------------- ("normalize")
            (prove_eq(decls, env, assumptions, x, z) => c)
        )
    }
}

#[track_caller]
fn prove_eq_normalized(
    decls: impl Upcast<Program>,
    env: impl Upcast<Env>,
    assumptions: impl Upcast<Wcs>,
    left: impl Upcast<Parameter>,
    right: impl Upcast<Parameter>,
) -> ProvenSet<Constraints> {
    let decls: Program = decls.upcast();
    let assumptions: Wcs = assumptions.upcast();
    let initial = Constrained::none(env, (left.upcast(), right.upcast()));
    let mut pending = VecDeque::from([(initial, ProofTree::leaf("normalized equality"), false)]);
    let mut visited = Set::new();
    let mut results = Map::new();

    while let Some((Constrained((left, right), mut c), tree, normalized)) = pending.pop_front() {
        let (assumptions, mut left, mut right) =
            c.substitution().apply((&assumptions, left, right));
        if right < left {
            std::mem::swap(&mut left, &mut right);
        }
        if (&assumptions, &left).size() > decls.max_size
            || (&assumptions, &right).size() > decls.max_size
        {
            c = c.ambiguous();
        }
        if !visited.insert(Constrained((left.clone(), right.clone()), c.clone())) {
            continue;
        }
        if !c.known_true {
            results.insert(
                c,
                ProofTree::new("normalized equality", Some("ambiguous"), vec![tree]),
            );
            continue;
        }
        if normalized {
            for (next, matched) in
                prove_syntactically_eq(&decls, c.env(), &assumptions, &left, &right).iter()
            {
                let result = c.seq(next);
                let proof = ProofTree::new(
                    "normalized equality",
                    Some("match"),
                    vec![tree.clone(), matched],
                );
                if result.unconditionally_true() {
                    return ProvenSet::singleton((result, proof));
                }
                results.insert(result, proof);
            }
        }
        for (index, parameter) in [&left, &right].into_iter().enumerate() {
            for (Constrained(q, next), step) in
                prove_normalize(&decls, c.env(), &assumptions, parameter).iter()
            {
                let c = c.seq(next);
                let pair = if index == 0 {
                    (q, right.clone())
                } else {
                    (left.clone(), q)
                };
                let pair = c.substitution().apply(pair);
                pending.push_back((
                    Constrained(pair, c),
                    ProofTree::new(
                        "normalized equality",
                        Some("step"),
                        vec![tree.clone(), step],
                    ),
                    true,
                ));
            }
        }
    }

    if results.is_empty() {
        ProvenSet::failed(
            "normalized equality",
            FailureLocation::caller(),
            "no matching normalized forms",
        )
    } else {
        ProvenSet::proven(results)
    }
}

judgment_fn! {
    pub fn prove_existential_var_eq(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        v: ExistentialVar,
        b: Parameter,
    ) => Constraints {
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
            (equate_variable(decls, env, assumptions, v, t) => c)
            ----------------------------- ("existential-nonvar")
            (prove_existential_var_eq(decls, env, assumptions, v, t) => c)
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
            (prove_existential_var_eq(_decls, env, _assumptions, l, Variable::ExistentialVar(r)) => (env, (b, a)))
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
            (prove_existential_var_eq(_decls, env, _assumptions, v, Variable::UniversalVar(p)) => (env, (v, p)))
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
) -> ProvenSet<Constraints> {
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
        .collect();

    tracing::debug!("equated: constraints={:?}, goals={:?}", constraints, goals);

    prove_after(decls, constraints, assumptions, goals)
}
