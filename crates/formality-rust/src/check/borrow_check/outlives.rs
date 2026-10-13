use crate::check::borrow_check::env::TypeckEnv;
use crate::check::borrow_check::flow_state::PendingOutlives;

use crate::grammar::{Parameter, Predicate, Variable, Wcs};
use crate::prove::prove;
use formality_core::{judgment_fn, visit::CoreVisit, Set, Upcast};

judgment_fn! {
    /// Verify that all pending outlives constraints between universal lifetime variables
    /// can be proven from the function's where-clause assumptions.
    pub(crate) fn verify_universal_outlives(
        env: TypeckEnv,
        assumptions: Wcs,
        outlives: Set<PendingOutlives>,
    ) => () {
        debug(env, assumptions, outlives)

        // Every universal the constraints mention, the fn's own and the
        // placeholders of a `for<..>` bound that a proof left constraints on.
        (
            (for_all(v in universals_mentioned(&env, &outlives))
                (only_assumed_outlives(env, assumptions, outlives, v) => ()))
            --- ("verify_universal_outlives")
            (verify_universal_outlives(env, assumptions, outlives) => ())
        )
    }
}

judgment_fn! {
    /// For existential lifetimes, trivially succeeds.
    /// For universal lifetimes, checks all transitively outlived variables can be proven from assumptions.
    fn only_assumed_outlives(
        env: TypeckEnv,
        assumptions: Wcs,
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
        assumptions: Wcs,
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
            (prove(&env.program, &env.env, assumptions, Predicate::outlives(param_a, var_b)) => c)
            (if c.unconditionally_true())
            --- ("universal target")
            (can_outlive(env, assumptions, _outlives, param_a, var_b: Variable) => ())
        )
    }
}

/// The variables of `env`, plus the universal variables `outlives` mention
/// that are not among them: the placeholders of a `for<..>` bound whose
/// proof deferred a constraint on them, which nothing can discharge.
fn universals_mentioned(env: &TypeckEnv, outlives: &Set<PendingOutlives>) -> Vec<Variable> {
    let mut vars: Vec<Variable> = env.env.variables().to_vec();
    for v in outlives.free_variables() {
        if v.is_universal() && !vars.contains(&v) {
            vars.push(v);
        }
    }
    vars
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
