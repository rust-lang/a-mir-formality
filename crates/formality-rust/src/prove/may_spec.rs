//! Deciding `may_spec(WC)`: the obligation at an `if impls WC` and at a call
//! to a fn declaring `where may_spec(WC)`. It holds if `WC` is decided: by a
//! `may_spec` bound in scope (`decide_by_bound`), or outright (`decide`). See
//! the "Branch specialization" chapter of the book.

use crate::grammar::{ParameterKind, Predicate, Wc, Wcs};
use crate::prove::{decls::Program, env::Env, prove, Constraints};
use formality_core::judgment_fn;
use formality_core::visit::CoreVisit;

/// The `may_spec` bounds among `assumptions`.
pub fn may_spec_bounds(assumptions: &Wcs) -> Vec<Wc> {
    assumptions
        .iter()
        .filter_map(|wc| match wc {
            Wc::Predicate(Predicate::MaySpec(bound)) => Some(bound.to_wc()),
            _ => None,
        })
        .collect()
}

/// Does `wc` mention no type or const parameter? (Lifetimes never select
/// an impl.)
pub fn is_closed(wc: &Wc) -> bool {
    wc.free_variables()
        .iter()
        .all(|v| v.kind() == ParameterKind::Lt)
}

judgment_fn! {
    /// `goal` is a `may_spec` bound in scope.
    pub fn decide_by_bound(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        goal: Wc,
    ) => Constraints {
        debug(goal, assumptions, env)

        (
            (let bounds = may_spec_bounds(&assumptions))
            (if !bounds.is_empty())!
            (if bounds.contains(&goal))
            ----------------------------- ("same bound")
            (decide_by_bound(_decls, env, assumptions, goal) => Constraints::none(env))
        )
    }
}

/// No new pending outlives obligation and no equation of a body region
/// variable in `c`?
fn leaves_no_region_constraint(env: &Env, c: &Constraints) -> bool {
    c.env().pending().len() == env.pending().len() && c.substitution().is_empty()
}

/// Is `goal` provable with every region constraint deferred to the borrow
/// checker? If not, no choice of lifetimes makes it hold.
fn provable_with_regions_deferred(
    decls: &Program,
    env: &Env,
    assumptions: &Wcs,
    goal: &Wc,
) -> bool {
    prove(
        decls,
        env.with_allow_pending_outlives(true),
        assumptions,
        goal,
    )
    .is_proven()
}

judgment_fn! {
    /// `goal` is decided: by a `may_spec` bound in scope, or it holds, or it
    /// is closed and does not hold.
    pub fn decide(
        decls: Program,
        env: Env,
        assumptions: Wcs,
        goal: Wc,
    ) => Constraints {
        debug(goal, assumptions, env)

        (
            (decide_by_bound(decls, env, assumptions, goal) => c)
            ----------------------------- ("by bound")
            (decide(decls, env, assumptions, goal) => c)
        )

        // Holds, with no region constraint left behind (strict).
        (
            (prove(decls, env, assumptions, goal) => c)
            (if leaves_no_region_constraint(&env, &c))
            ----------------------------- ("holds")
            (decide(decls, env, assumptions, goal) => c)
        )

        // Closed (no type or const parameter) and unprovable even with region
        // constraints deferred: no instantiation or downstream impl can change
        // that. Unprovable over a type parameter is not "no".
        (
            (if is_closed(&goal))!
            (if !provable_with_regions_deferred(&decls, &env, &assumptions, &goal))
            ----------------------------- ("does not hold")
            (decide(decls, env, assumptions, goal) => Constraints::none(env))
        )
    }
}
