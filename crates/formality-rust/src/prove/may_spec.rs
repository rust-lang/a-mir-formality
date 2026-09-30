//! Deciding `may_spec(T: Trait)`: the obligation at an `if impls T: Trait`
//! and at a call to a fn declaring `where may_spec(T: Trait)`. It holds if it
//! is a `may_spec` bound in scope (an assumption, like any where-clause), or
//! if the bound is decided (`decide`). See the "Branch specialization"
//! chapter of the book.
//!
//! A higher-ranked bound, `for<'a> may_spec(T: Trait<'a>)`, is decided like
//! any `for<..>` goal: for a fresh universal `'a`.

use crate::grammar::{FeatureGateName, ParameterKind, Predicate, Predicates, TraitRef};
use crate::prove::lifetimes::erase_lifetimes_in_predicate;
use crate::prove::{decls::Program, env::Env, prove, Constraints};
use formality_core::visit::CoreVisit;
use formality_core::{judgment_fn, ProvenSet, Upcast};

/// Does `term` mention no type or const parameter? (Lifetimes never select
/// an impl.)
pub fn is_closed(term: &impl CoreVisit<crate::FormalityLang>) -> bool {
    term.free_variables()
        .iter()
        .all(|v| v.kind() == ParameterKind::Lt)
}

/// May a decision leave the region constraints `c` (from `env`) to the
/// borrow checker? Strict: no. `spec_commit_and_verify`: yes.
fn allowed_by_mode(decls: &Program, env: &Env, c: &Constraints) -> bool {
    decls.feature_gate_enabled(&FeatureGateName::SpecCommitAndVerify)
        || leaves_no_region_constraint(env, c)
}

/// No new pending outlives obligation and no equation of a body region
/// variable in `c`?
fn leaves_no_region_constraint(env: &Env, c: &Constraints) -> bool {
    c.env().pending().len() == env.pending().len() && c.substitution().is_empty()
}

/// Does `goal` hold once every lifetime is erased? This is how codegen
/// evaluates the bound of an `if impls` (`decide_at_codegen`): a region
/// constraint on an erased lifetime counts as satisfied, so none may be left.
///
/// `decide` asserts that its answers agree with it: a bound decided as
/// holding holds after erasure, and one decided as not holding does not, so
/// codegen, which has nothing else to go on, takes the branch type checking
/// decided. (An ambiguous answer, as on overflow, is no decision.)
pub fn holds_after_erasure(
    decls: &Program,
    env: &Env,
    assumptions: &Predicates,
    goal: impl Upcast<Predicate>,
) -> bool {
    let goal: Predicate = goal.upcast();
    let assumptions: Predicates = assumptions
        .iter()
        .map(|wc| erase_lifetimes_in_predicate(&wc))
        .collect();
    let env = env.with_allow_pending_outlives(false);
    match prove(
        decls,
        &env,
        assumptions,
        erase_lifetimes_in_predicate(&goal),
    )
    .into_map()
    {
        Ok(solutions) => solutions.keys().any(|c| {
            c.known_true
                && c.substitution.is_empty()
                && c.env.pending().len() == env.pending().len()
        }),
        Err(_) => false,
    }
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

        // Holds, with region constraints as the mode allows.
        (
            (prove(decls, env, assumptions, goal) => c)
            (if allowed_by_mode(&decls, &env, &c))
            (assert !c.known_true || holds_after_erasure(&decls, c.env(), &assumptions, c.substitution().apply(goal)))
            ----------------------------- ("holds")
            (decide(decls, env, assumptions, goal) => c)
        )

        // Closed (no type or const parameter) and unprovable, even with region
        // constraints deferred: no choice of lifetimes or downstream impl can
        // change that. Unprovable over a type parameter is not "no".
        (
            (if is_closed(&goal))!
            (if !prove_for_some_lifetimes(&decls, &env, &assumptions, &goal).is_proven())
            (assert !holds_after_erasure(&decls, &env, &assumptions, goal))
            ----------------------------- ("does not hold")
            (decide(decls, env, assumptions, goal) => Constraints::none(env))
        )
    }
}
