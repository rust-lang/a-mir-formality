mod constraints;
mod forall;
mod prove_after;
mod prove_apr;
mod prove_apr_via;
mod prove_eq;
mod prove_normalize;
mod prove_wc;
mod prove_wc_list;
mod subst;

pub use constraints::Constraints;
use formality_types::{
    cast::Upcast,
    collections::Set,
    grammar::{Binder, Wcs},
    set,
    visit::Visit,
};
use tracing::Level;

use crate::program::Program;

use self::prove_wc_list::prove_wc_list;

/// Top-level entry point for proving things; other rules recurse to this one.
pub fn prove(
    program: impl Upcast<Program>,
    assumptions: impl Upcast<Wcs>,
    goal: impl Upcast<Wcs>,
) -> Set<Binder<Constraints>> {
    let program: Program = program.upcast();
    let assumptions: Wcs = assumptions.upcast();
    let goal: Wcs = goal.upcast();

    let span = tracing::span!(Level::DEBUG, "prove", ?goal, ?assumptions);
    let _guard = span.enter();

    let term_in = (&assumptions, &goal);
    let fv_in = term_in.free_variables();
    if term_in.size() > program.max_size {
        tracing::debug!(
            "term has size {} which exceeds max size of {}",
            term_in.size(),
            program.max_size
        );
        return set![Binder::dummy(Constraints::default().ambiguous())];
    }

    let cs = prove_wc_list(program, assumptions, goal);

    cs.assert_valid();
    cs.free_variables()
        .iter()
        .for_each(|fv| assert!(fv_in.contains(&fv)));

    tracing::debug!(?cs);

    cs
}
