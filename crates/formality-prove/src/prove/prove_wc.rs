use formality_macros::term;
use formality_types::{
    cast::Upcast,
    collections::Set,
    grammar::{WcData, Wcs},
    judgment,
    judgment::Judgment,
    visit::Visit,
};

use crate::{
    program::Program,
    prove::{forall::constraints_visible_from_universe, prove_apr::prove_apr},
};

use super::ConstraintSet;

pub fn prove_wc(
    program: impl Upcast<Program>,
    assumptions: impl Upcast<Wcs>,
    goal: impl Upcast<WcData>,
) -> Set<ConstraintSet> {
    ProveWc(program.upcast(), assumptions.upcast(), goal.upcast()).apply()
}

#[term]
struct ProveWc(Program, Wcs, WcData);

judgment! {
    (ProveWc => ConstraintSet)

    (
        (prove_apr(program, assumptions, a) => c)
        --- ("atomic")
        (ProveWc(program, assumptions, WcData::Atomic(a)) => c)
    )

    (
        (let u = assumptions.max_universe())
        (let p1 = binder.instantiate_universally(&assumptions))
        (prove_wc(program, assumptions, p1) => c)
        (let c1 = constraints_visible_from_universe(u, c))
        --- ("forall")
        (ProveWc(program, assumptions, WcData::ForAll(binder)) => c1)
    )

    (
        (prove_wc(program, all![assumptions, p1], p2) => c)
        --- ("implies")
        (ProveWc(program, assumptions, WcData::Implies(p1, p2)) => c)
    )
}
