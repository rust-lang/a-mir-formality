mod constraints;
mod env;
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
    let env0: Env = env.upcast();
    let assumptions: Wcs = assumptions.upcast();
    let goal: Wcs = goal.upcast();

    let span = tracing::span!(Level::DEBUG, "prove", ?goal, ?assumptions);
    let _guard = span.enter();

    let term_in = (&assumptions, &goal);
    if term_in.size() > program.max_size {
        tracing::debug!(
            "term has size {} which exceeds max size of {}",
            term_in.size(),
            program.max_size
        );
        return set![Constraints::none(env0).ambiguous()];
    }

    env0.assert_encloses(term_in);

    let result_set = prove_wc_list(program, &env0, assumptions, goal);

    result_set.iter().for_each(|constraints1| {
        constraints1.assert_valid_extension_of(&env0);
    });

    tracing::debug!(?result_set);

    result_set
}
