mod combinators;
mod constraints;
mod env;
mod is_local;
mod minimize;
mod prove_after;
mod prove_eq;
mod prove_normalize;
mod prove_via;
mod prove_wc;
mod prove_wc_list;
mod prove_wf;

pub use constraints::Constraints;
use formality_core::visit::CoreVisit;
use formality_core::{ProvenSet, Upcast};
use formality_types::grammar::Wcs;
use tracing::Level;

use crate::decls::Decls;

pub use self::env::{Bias, Env};
use self::prove_wc_list::prove_wc_list;

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

    let (env, (assumptions, goal), min) = minimize::minimize(env, (assumptions, goal));

    let span = tracing::span!(Level::DEBUG, "prove", ?goal, ?assumptions, ?env, ?decls);
    let _guard = span.enter();

    let term_in = (&assumptions, &goal);
    if term_in.size() > decls.max_size {
        tracing::debug!(
            "term has size {} which exceeds max size of {}",
            term_in.size(),
            decls.max_size
        );
        return ProvenSet::singleton(Constraints::none(env).ambiguous());
    }

    assert!(env.encloses(term_in));

    let result_set = prove_wc_list(decls, &env, assumptions, goal);

    tracing::debug!(?result_set);

    result_set.map(|r| {
        assert!(r.is_valid_extension_of(&env));
        min.reconstitute(r)
    })
}
