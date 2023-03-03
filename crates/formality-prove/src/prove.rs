mod constraints;
mod forall;
mod prove_after;
mod prove_apr;
mod prove_apr_via;
mod prove_eq;
mod prove_wc;
mod prove_wc_list;
mod subst;

pub use constraints::Constraints;
use formality_types::{
    cast::Upcast,
    collections::Set,
    grammar::{Binder, Wcs},
    visit::Visit,
};

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

    let fv_in = (&assumptions, &goal).free_variables();

    let cs = prove_wc_list(program, assumptions, goal);

    cs.free_variables()
        .iter()
        .for_each(|fv| assert!(fv_in.contains(&fv)));

    cs
}
