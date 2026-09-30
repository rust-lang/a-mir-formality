//! Deciding `may_spec(WC)`: the obligation at an `if impls WC` and at a call
//! to a fn declaring `where may_spec(WC)`. It holds if `WC` is decided: by a
//! `may_spec` bound in scope (`decide_by_bound`), or outright (`decide`). See
//! the "Branch specialization" chapter of the book.

use crate::grammar::{Binder, Fallible, FeatureGateName, Lt, ParameterKind, Predicate, Wc, Wcs};
use crate::prove::lifetimes::map_lifetimes_in_trait_ref;
use crate::prove::{decls::Program, env::Env, prove, Constraints};
use anyhow::bail;
use formality_core::visit::CoreVisit;
use formality_core::{judgment_fn, term, Upcast};

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

        // Its impls do not depend on lifetimes, so codegen decides exactly after
        // erasure (see `check_always_applicable_impl`).
        (
            (if decls.is_always_applicable_trait(&tr.trait_id))!
            ----------------------------- ("always applicable trait")
            (decide(decls, env, assumptions, Predicate::IsImplemented(tr)) => Constraints::none(env))
        )

        (
            (decide_by_bound(decls, env, assumptions, goal) => c)
            ----------------------------- ("by bound")
            (decide(decls, env, assumptions, goal) => c)
        )

        // Holds, with region constraints as the mode allows.
        (
            (prove(decls, env, assumptions, goal) => c)
            (if allowed_by_mode(&decls, &env, &c))
            (holds_for_all_lifetimes_if_required(decls, c.env(), assumptions, c.substitution().apply(goal)) => ())
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

judgment_fn! {
    /// Under `spec_always_applicable`, a bound decided as holding must hold
    /// for every choice of the lifetimes in it.
    fn holds_for_all_lifetimes_if_required(
        decls: Program,
        env: Env,
        assumptions: Wcs,
        goal: Wc,
    ) => () {
        debug(goal, assumptions, env)

        (
            (if !decls.feature_gate_enabled(&FeatureGateName::SpecAlwaysApplicable))!
            ----------------------------- ("not required")
            (holds_for_all_lifetimes_if_required(decls, _env, _assumptions, _goal) => ())
        )

        (
            (if decls.feature_gate_enabled(&FeatureGateName::SpecAlwaysApplicable))!
            (holds_for_all_lifetimes(decls, env, assumptions, goal, LifetimeSelection::All) => _)
            ----------------------------- ("always applicable")
            (holds_for_all_lifetimes_if_required(decls, env, assumptions, goal) => ())
        )
    }
}

/// `#![feature(spec_bail_on_regions)]`: no `may_spec` needed; codegen
/// decides, and a lifetime-dependent bound counts as not holding.
pub fn bail_on_regions(program: &Program) -> bool {
    program.feature_gate_enabled(&FeatureGateName::SpecBailOnRegions)
}

/// The lifetimes of a bound `holds_for_all_lifetimes` quantifies over.
#[term]
pub enum LifetimeSelection {
    /// Those erased at codegen (`spec_bail_on_regions`; compare rustc's
    /// `TypingMode::Reflection`).
    #[grammar(erased)]
    Erased,
    /// All of them (`spec_always_applicable`).
    #[grammar(all)]
    All,
}

judgment_fn! {
    /// `goal` holds for *every* choice of the selected lifetimes: each is
    /// replaced by a fresh universal lifetime, and the goal must then be
    /// provable with no region constraint.
    pub fn holds_for_all_lifetimes(
        decls: Program,
        env: Env,
        assumptions: Wcs,
        goal: Wc,
        selection: LifetimeSelection,
    ) => Constraints {
        debug(goal, selection, assumptions, env)

        (
            // No region constraint may be left, so none is deferred.
            (let (env, goal) = quantify_lifetimes(&env.with_allow_pending_outlives(false), &goal, &selection)?)
            (prove(decls, env, assumptions, goal) => c)
            (if c.unconditionally_true())
            ----------------------------- ("holds for all lifetimes")
            (holds_for_all_lifetimes(decls, env, assumptions, goal, selection) => c)
        )
    }
}

/// `goal` with each selected lifetime replaced by a fresh universal lifetime
/// of `env` (under a `for<..>`, the bound lifetimes are left alone).
fn quantify_lifetimes(env: &Env, goal: &Wc, selection: &LifetimeSelection) -> Fallible<(Env, Wc)> {
    let mut env = env.clone();
    let mut fresh = |lt: &Lt| -> Lt {
        let selected = match selection {
            LifetimeSelection::Erased => matches!(lt, Lt::Erased),
            LifetimeSelection::All => true,
        };
        if selected {
            env.fresh_universal(ParameterKind::Lt).upcast()
        } else {
            lt.clone()
        }
    };
    let goal = replace_lifetimes_in_wc(goal, &mut fresh)?;
    Ok((env, goal))
}

fn replace_lifetimes_in_wc(wc: &Wc, f: &mut impl FnMut(&Lt) -> Lt) -> Fallible<Wc> {
    Ok(match wc {
        Wc::Predicate(Predicate::IsImplemented(tr)) => {
            Predicate::is_implemented(map_lifetimes_in_trait_ref(tr, f)).upcast()
        }
        Wc::Predicate(Predicate::NotImplemented(tr)) => {
            Predicate::not_implemented(map_lifetimes_in_trait_ref(tr, f)).upcast()
        }
        Wc::ForAll(binder) => {
            let (vars, wc) = binder.open();
            Wc::for_all(Binder::new(&vars, replace_lifetimes_in_wc(&wc, f)?))
        }
        other => bail!("cannot decide `{other:?}` for all lifetimes"),
    })
}
