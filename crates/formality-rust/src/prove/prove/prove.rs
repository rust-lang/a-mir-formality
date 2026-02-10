pub mod combinators;
mod constraints;
mod env;
mod is_local;
mod minimize;
mod negation;
mod prove_after;
mod prove_const_has_type;
mod prove_eq;
pub mod prove_normalize;
mod prove_outlives;
mod prove_sub;
mod prove_via;
mod prove_wc;
mod prove_wc_list;
mod prove_wf;

use crate::types::grammar::Wcs;
pub use constraints::Constraints;
use formality_core::judgment::{EachProof, FailedRule, ProofTree};
use formality_core::visit::CoreVisit;
use formality_core::{map, set, ProvenSet, Upcast};
use tracing::Level;

use crate::prove::prove::decls::Decls;

pub use self::env::{Bias, Env};
use self::prove_wc_list::prove_wc_list;
pub use negation::{is_definitely_not_proveable, may_not_be_provable, negation_via_failure};

/// Top-level entry point for proving things; other rules recurse to this one.
pub fn prove(
    decls: impl Upcast<Decls>,
    env: impl Upcast<Env>,
    assumptions: impl Upcast<Wcs>,
    goal: impl Upcast<Wcs>,
) -> ProvenSet<Constraints> {
    let decls: Decls = decls.upcast();
    let env: Env = env.upcast();
    let assumptions: Wcs = assumptions.upcast();
    let goal: Wcs = goal.upcast();

    // "Minimize" the env/assumptions/goals so that we better detect cycles.
    let (env, (assumptions, goal), min) = minimize::minimize(env, (assumptions, goal));

    // Establish context for debugging/tracing logs.
    let span = tracing::span!(Level::DEBUG, "prove", ?goal, ?assumptions, ?env, ?decls);
    let _guard = span.enter();

    // Fail if the terms are getting too large ("overflow detection").
    // This is meant to capture complex recursion cycles that will never terminate but also
    // never reach a (simple) cycle, e.g., proving `A: Foo` requires proving `Vec<A>: Foo`
    // requires proving `Vec<Vec<A>>: Foo` etc.
    //
    // In the compiler we use recursion depth instead. We avoid recursion depth because it requires
    // knowing the context in which the proof occurs.
    let term_in = (&assumptions, &goal);
    if term_in.size() > decls.max_size {
        tracing::debug!(
            "term has size {} which exceeds max size of {}",
            term_in.size(),
            decls.max_size
        );
        return ProvenSet::singleton((
            Constraints::none(env).ambiguous(),
            ProofTree::leaf("max term size exceeded"),
        ));
    }

    // Assert the term we are trying to prove should not have any variables that are not in the environment.
    assert!(env.encloses(term_in));

    // Call `prove_wc_list` to do the real work.
    struct ProveFailureLabel(String);
    let label = ProveFailureLabel(format!(
        "prove {{ goal: {goal:?}, assumptions: {assumptions:?}, env: {env:?}, decls: {decls:?} }}"
    ));
    impl std::fmt::Debug for ProveFailureLabel {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str(&self.0)
        }
    }
    let mut results = map![];
    let result_set = if let Err(e) =
        prove_wc_list(decls, &env, assumptions, goal).each_proof(|(result, proof_tree)| {
            results.insert(result, proof_tree);
        }) {
        ProvenSet::failed_rules(label, set![FailedRule::new(e)])
    } else {
        ProvenSet::proven(results)
    };

    tracing::debug!(?result_set);

    // Map the results back to the "unminimized" form ("reconstitute").
    let maxified = result_set.map(|(r, proof_tree)| {
        assert!(r.is_valid_extension_of(&env));
        (min.reconstitute(r), proof_tree)
    });

    tracing::debug!(?maxified);

    maxified
}
