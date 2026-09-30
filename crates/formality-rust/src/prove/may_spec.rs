//! Deciding `may_spec(T: Trait)`: the obligation at an `if impls T: Trait`
//! and at a call to a fn declaring `where may_spec(T: Trait)`. It holds if it
//! is a `may_spec` bound in scope (an assumption, like any where-clause), or
//! if the bound is decided (`decide`). See the "Branch specialization"
//! chapter of the book.
//!
//! A higher-ranked bound, `for<'a> may_spec(T: Trait<'a>)`, is decided like
//! any `for<..>` goal: for a fresh universal `'a`.

use crate::grammar::{ParameterKind, Predicates, TraitRef};
use crate::prove::{decls::Program, env::Env, prove, Constraints};
use formality_core::visit::CoreVisit;
use formality_core::{judgment_fn, ProvenSet};

/// Does `term` mention no type or const parameter? (Lifetimes never select
/// an impl.)
pub fn is_closed(term: &impl CoreVisit<crate::FormalityLang>) -> bool {
    term.free_variables()
        .iter()
        .all(|v| v.kind() == ParameterKind::Lt)
}

/// No new pending outlives obligation and no equation of a body region
/// variable in `c`?
fn leaves_no_region_constraint(env: &Env, c: &Constraints) -> bool {
    c.env().pending().len() == env.pending().len() && c.substitution().is_empty()
}

/// Prove `goal` with every region constraint deferred to the borrow checker.
/// If there is no proof, no choice of lifetimes makes `goal` hold.
fn prove_for_some_lifetimes(
    decls: &Program,
    env: &Env,
    assumptions: &Predicates,
    goal: &TraitRef,
) -> ProvenSet<Constraints> {
    prove(
        decls,
        env.with_allow_pending_outlives(true),
        assumptions,
        goal,
    )
}

judgment_fn! {
    /// `goal` is decided: it holds, or it is closed and does not hold.
    pub fn decide(
        decls: Program,
        env: Env,
        assumptions: Predicates,
        goal: TraitRef,
    ) => Constraints {
        debug(goal, assumptions, env)

        // Holds, with no region constraint left behind (strict).
        (
            (prove(decls, env, assumptions, goal) => c)
            (if leaves_no_region_constraint(&env, &c))
            ----------------------------- ("holds")
            (decide(decls, env, assumptions, goal) => c)
        )

        // Closed (no type or const parameter) and unprovable, even with region
        // constraints deferred: no choice of lifetimes or downstream impl can
        // change that. Unprovable over a type parameter is not "no".
        (
            (if is_closed(&goal))!
            (if !prove_for_some_lifetimes(&decls, &env, &assumptions, &goal).is_proven())
            ----------------------------- ("does not hold")
            (decide(decls, env, assumptions, goal) => Constraints::none(env))
        )
    }
}
