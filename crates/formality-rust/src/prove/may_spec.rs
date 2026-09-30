//! Deciding `may_spec(T: Trait)`: the obligation at an `if impls T: Trait`
//! and at a call to a fn declaring `where may_spec(T: Trait)`. It holds if
//! the bound is decided: by a `may_spec` bound in scope (`decide_by_bound`),
//! or outright (`decide`). See the "Branch specialization" chapter of the
//! book.
//!
//! A higher-ranked bound, `for<'a> may_spec(T: Trait<'a>)`, is decided like
//! any `for<..>` goal: for a fresh universal `'a`.

use crate::grammar::{
    AtomicPredicate, FeatureGateName, ParameterKind, Predicate, Predicates, TraitRef,
    VarSubstitution,
};
use crate::prove::lifetimes::{erase_lifetimes_in_predicate, map_lifetimes_in_predicate};
use crate::prove::{
    decls::Program, env::Env, negation::is_definitely_not_proveable, prove, prove_via::prove_via,
    Constraints,
};
use formality_core::visit::CoreVisit;
use formality_core::{judgment_fn, ProvenSet, Upcast};

/// The `may_spec` bounds among `assumptions`, under their `for<..>` if any.
pub fn may_spec_bounds(assumptions: &Predicates) -> Vec<Predicate> {
    assumptions
        .iter()
        .filter(|wc| is_may_spec_bound(wc))
        .collect()
}

fn is_may_spec_bound(wc: &Predicate) -> bool {
    match wc {
        Predicate::AtomicPredicate(AtomicPredicate::MaySpec(_)) => true,
        Predicate::ForAll(binder) => is_may_spec_bound(binder.peek()),
        _ => false,
    }
}

/// Does `term` mention no type or const parameter? (Lifetimes never select
/// an impl.)
pub fn is_closed(term: &impl CoreVisit<crate::FormalityLang>) -> bool {
    term.free_variables()
        .iter()
        .all(|v| v.kind() == ParameterKind::Lt)
}

judgment_fn! {
    /// `goal` is an instance of a `may_spec` bound in scope, up to unification
    /// of the parameters (`prove_via`, as for any where-clause in scope). A
    /// `for<..>` bound decides each instance: the caller's "yes" covers every
    /// instantiation, and its "no" means no impl matches at any (a bound that
    /// fails only for some lifetimes is never decided "no"). The constraints
    /// that leaves are subject to the mode, like a proof's.
    pub fn decide_by_bound(
        decls: Program,
        env: Env,
        assumptions: Predicates,
        goal: TraitRef,
    ) => Constraints {
        debug(goal, assumptions, env)

        (
            (let bounds = may_spec_bounds(&assumptions))
            (if !bounds.is_empty())!
            (bound in bounds)
            (prove_via(decls, env, assumptions, bound, AtomicPredicate::may_spec(goal)) => c)
            (if allowed_by_mode(&decls, &env, &c))
            ----------------------------- ("bound unifies")
            (decide_by_bound(decls, env, assumptions, goal) => c)
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

/// `env`, `assumptions` and `goal` with each region variable of the body (an
/// existential lifetime) replaced by a fresh universal one. Lifetimes never
/// select an impl, and `prove_for_some_lifetimes` defers every region
/// constraint, so the search is unchanged.
fn universal_lifetimes(
    env: &Env,
    assumptions: &Predicates,
    goal: &TraitRef,
) -> (Env, Predicates, TraitRef) {
    let mut fresh = env.clone();
    let vs: VarSubstitution = env
        .variables()
        .iter()
        .filter(|v| v.is_existential() && v.kind() == ParameterKind::Lt)
        .map(|&v| (v, fresh.fresh_universal(ParameterKind::Lt)))
        .collect();
    (env.substitute(&vs), vs.apply(assumptions), vs.apply(goal))
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
    /// `goal` is decided. Each rule is one way to decide it; of several
    /// answers, the type checker takes the one with the weakest region
    /// constraints (`TypeckEnv::prove_judgment`).
    pub fn decide(
        decls: Program,
        env: Env,
        assumptions: Predicates,
        goal: TraitRef,
    ) => Constraints {
        debug(goal, assumptions, env)

        (
            (decide_by_bound(decls, env, assumptions, goal) => c)
            ----------------------------- ("by bound")
            (decide(decls, env, assumptions, goal) => c)
        )

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

        // Over a type parameter, but coherence rules out every impl, present or
        // future (`is_definitely_not_proveable`; needs an env without inference
        // variables, so the body's region variables are first made universal).
        // FIXME: formality's coherence does not know fundamental types, so
        // `&'a T: Bar` is decided "no" although a downstream crate may
        // `impl Bar for &'static Local`.
        (
            (if !is_closed(&goal))!
            (let (coherence_env, coherence_assumptions, coherence_goal) = universal_lifetimes(&env, &assumptions, &goal))
            (if coherence_env.only_universal_variables())
            (is_definitely_not_proveable(&coherence_env, &coherence_assumptions, coherence_goal, |env, assumptions, goal| prove_for_some_lifetimes(&decls, &env, &assumptions, &goal)) => c)
            (if c.unconditionally_true())
            (assert !holds_after_erasure(&decls, &env, &assumptions, goal))
            ----------------------------- ("does not hold, by coherence")
            (decide(decls, env, assumptions, goal) => Constraints::none(env))
        )
    }
}

/// `#![feature(spec_bail_on_regions)]`: no `may_spec` needed; codegen
/// decides, and a lifetime-dependent bound counts as not holding.
pub fn bail_on_regions(program: &Program) -> bool {
    program.feature_gate_enabled(&FeatureGateName::SpecBailOnRegions)
}

judgment_fn! {
    /// `goal` holds for *every* choice of its lifetimes: each occurrence of a
    /// lifetime is replaced by a fresh universal lifetime, and the goal must
    /// then be provable with no region constraint. Two occurrences of one
    /// lifetime are two choices, as after erasure (where every lifetime is
    /// `'erased`) they cannot be told apart; compare rustc's
    /// `TypingMode::Reflection`, and its always-applicable rule against a
    /// parameter named twice.
    pub fn holds_for_all_lifetimes(
        decls: Program,
        env: Env,
        assumptions: Predicates,
        goal: Predicate,
    ) => Constraints {
        debug(goal, assumptions, env)

        (
            // No region constraint may be left, so none is deferred.
            (let (env, goal) = quantify_lifetimes(&env.with_allow_pending_outlives(false), &goal))
            (prove(decls, env, assumptions, goal) => c)
            (if c.unconditionally_true())
            ----------------------------- ("holds for all lifetimes")
            (holds_for_all_lifetimes(decls, env, assumptions, goal) => c)
        )
    }
}

/// `goal` with each occurrence of a lifetime replaced by a fresh universal
/// lifetime of `env`.
fn quantify_lifetimes(env: &Env, goal: &Predicate) -> (Env, Predicate) {
    let mut env = env.clone();
    let goal = map_lifetimes_in_predicate(goal, &mut |_| {
        env.fresh_universal(ParameterKind::Lt).upcast()
    });
    (env, goal)
}
