use formality_macros::term;
use formality_types::{cast::Upcast, collections::Set, grammar::Wcs, judgment, judgment::Judgment};

use crate::{program::Program, prove::prove_after::prove_after};

use super::{prove_wc::prove_wc, ConstraintSet};

pub fn prove_wc_list(
    program: impl Upcast<Program>,
    assumptions: impl Upcast<Wcs>,
    goal: impl Upcast<Wcs>,
) -> Set<ConstraintSet> {
    ProveWcs(program.upcast(), assumptions.upcast(), goal.upcast()).apply()
}

#[term]
struct ProveWcs(Program, Wcs, Wcs);

judgment! {
    (ProveWcs => ConstraintSet)

    (
        (if let None = wcs.split_first())
        --- ("none")
        (ProveWcs(_env, _assumptions, wcs) => ConstraintSet::new())
    )

    (
        (if let Some((wc0, wcs1)) = wcs.split_first())
        (prove_wc(&program, &assumptions, wc0) => c1)
        (prove_after(&program, &assumptions, c1, &wcs1) => c2)
        --- ("some")
        (ProveWcs(program, assumptions, wcs) => c2)
    )
}
