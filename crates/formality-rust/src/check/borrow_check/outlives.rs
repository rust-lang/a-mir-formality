use crate::check::borrow_check::env::TypeckEnv;
use crate::check::borrow_check::flow_state::PendingOutlives;

use crate::grammar::{AtomicPredicate, Parameter, Predicates, Variable};
use crate::prove::prove;
use formality_core::{judgment_fn, Downcast, Set, Upcast};

judgment_fn! {
    /// Verify that all pending outlives constraints between universal lifetime variables
    /// can be proven from the function's where-clause assumptions.
    pub(crate) fn verify_universal_outlives(
        env: TypeckEnv,
        assumptions: Predicates,
        outlives: Set<PendingOutlives>,
    ) => () {
        debug(env, assumptions, outlives)

        (
            (for_all(v in env.env.variables())
                (only_assumed_outlives(env, assumptions, outlives, v) => ()))
            (for_all(po in outlives)
                (non_variable_outlives(env, assumptions, po.a.clone(), po.b.clone()) => ()))
            --- ("verify_universal_outlives")
            (verify_universal_outlives(env, assumptions, outlives) => ())
        )
    }
}

judgment_fn! {
    /// A pending `a: b` whose left side is not a variable, e.g. `<T as A>::Item: 'a`,
    /// is never the start of a walk in `only_assumed_outlives`, so it must be proven
    /// directly from the assumptions here (issue #530).
    fn non_variable_outlives(
        env: TypeckEnv,
        assumptions: Predicates,
        a: Parameter,
        b: Parameter,
    ) => () {
        debug(a, b, assumptions, env)

        // Variables on the left are handled by `only_assumed_outlives`.
        (
            (if let Some(_v) = a.downcast::<Variable>())
            --- ("variable on the left")
            (non_variable_outlives(_env, _assumptions, a, _b) => ())
        )

        // An existential target can still be inferred, so nothing to prove yet.
        (
            (if let None = a.downcast::<Variable>())
            (if let Some(v) = b.downcast::<Variable>())
            (if v.is_existential())
            --- ("existential target")
            (non_variable_outlives(_env, _assumptions, a, b) => ())
        )

        // Otherwise it must follow from the assumptions.
        (
            (if let None = a.downcast::<Variable>())
            (prove(&env.program, &env.env, assumptions, AtomicPredicate::outlives(a, b)) => c)
            (if c.unconditionally_true())
            --- ("non-variable left side must be proven")
            (non_variable_outlives(env, assumptions, a, b) => ())
        )
    }
}

judgment_fn! {
    /// For existential lifetimes, trivially succeeds.
    /// For universal lifetimes, checks all transitively outlived variables can be proven from assumptions.
    fn only_assumed_outlives(
        env: TypeckEnv,
        assumptions: Predicates,
        outlives: Set<PendingOutlives>,
        v: Variable,
    ) => () {
        debug(env, assumptions, outlives, v)

        // Non-universal variables (existentials) - trivially succeed
        (
            (if v.is_existential())!
            --- ("existential")
            (only_assumed_outlives(_env, _assumptions, _outlives, v) => ())
        )

        // Universal lifetime variables - check all transitively outlived
        (
            (if v.is_universal())!
            (for_all(param in transitively_outlived_by(env, outlives, v))
                (can_outlive(env, assumptions, outlives, v, param) => ()))
            --- ("universal lifetime")
            (only_assumed_outlives(env, assumptions, outlives, v) => ())
        )
    }
}

judgment_fn! {
    /// Check if lt_a can outlive param_b.
    /// For non-universal targets, trivially succeeds.
    /// For universal lifetime targets, must prove from assumptions.
    fn can_outlive(
        env: TypeckEnv,
        assumptions: Predicates,
        outlives: Set<PendingOutlives>,
        param_a: Parameter,
        param_b: Parameter,
    ) => () {
        debug(param_a, param_b, assumptions, env, outlives)

        trivial(param_a == param_b => ())

        // Prove that T: 'a -- implied because we're working over transitive outlives
        (
            (if var_b.is_existential())!
            --- ("existential target")
            (can_outlive(_env, _assumptions, _outlives, _param_a, var_b: Variable) => ())
        )

        // Prove that T: !a -- must prove from assumptions
        (
            (if var_b.is_universal())!
            (prove(&env.program, &env.env, assumptions, AtomicPredicate::outlives(param_a, var_b)) => c)
            (if c.unconditionally_true())
            --- ("universal target")
            (can_outlive(env, assumptions, _outlives, param_a, var_b: Variable) => ())
        )
    }
}

/// Given a region `r`, find a set of all regions `r1` where `r: r1` transitively
/// according to the `pending_outlives` in `env`.
pub fn transitively_outlived_by(
    _env: &TypeckEnv,
    pending_outlives: &Set<PendingOutlives>,
    start_lt: impl Upcast<Parameter>,
) -> Set<Parameter> {
    let start_lt = start_lt.upcast();
    let mut reachable = Set::new();

    reachable.insert(start_lt.clone());
    let mut worklist = vec![start_lt.clone()];

    while let Some(current) = worklist.pop() {
        for PendingOutlives { a, b } in pending_outlives.iter() {
            if *a == current {
                if reachable.insert(b.clone()) {
                    worklist.push(b.clone());
                }
            }
        }
    }

    reachable
}
