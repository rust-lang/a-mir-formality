mod constraints;
mod env;
mod minimize;
mod prove_after;
mod prove_apr;
mod prove_apr_via;
mod prove_eq;
mod prove_normalize;
mod prove_wc;
mod prove_wc_list;

pub use constraints::Constraints;
use formality_types::{cast::Upcast, collections::Set, grammar::Wcs, set, visit::Visit};
use tracing::Level;

use crate::program::Program;

pub use self::env::Env;
use self::prove_wc_list::prove_wc_list;

/// Top-level entry point for proving things; other rules recurse to this one.
pub fn prove(
    program: impl Upcast<Program>,
    env: impl Upcast<Env>,
    assumptions: impl Upcast<Wcs>,
    goal: impl Upcast<Wcs>,
) -> Set<Constraints> {
    let program: Program = program.upcast();
    let env: Env = env.upcast();
    let assumptions: Wcs = assumptions.upcast();
    let goal: Wcs = goal.upcast();

    let (env, (assumptions, goal), min) = minimize::minimize(env, (assumptions, goal));

    let span = tracing::span!(Level::DEBUG, "prove", ?goal, ?assumptions, ?env, ?program);
    let _guard = span.enter();

    let term_in = (&assumptions, &goal);
    if term_in.size() > program.max_size {
        tracing::debug!(
            "term has size {} which exceeds max size of {}",
            term_in.size(),
            program.max_size
        );
        return set![Constraints::none(env).ambiguous()];
    }

    env.assert_encloses(term_in);

    let result_set = prove_wc_list(program, &env, assumptions, goal);

    result_set.iter().for_each(|constraints1| {
        constraints1.assert_valid_extension_of(&env);
    });

    tracing::debug!(?result_set);

    result_set
        .into_iter()
        .map(|r| min.reconstitute(r))
        .collect()
}
