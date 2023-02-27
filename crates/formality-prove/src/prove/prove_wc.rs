use formality_macros::term;
use formality_types::{
    cast::Upcast,
    collections::Set,
    grammar::{WcData, WcList},
    judgment,
    judgment::Judgment,
};

use crate::{program::Program, prove::prove_apr::prove_apr};

use super::ConstraintSet;

pub fn prove_wc(
    program: impl Upcast<Program>,
    assumptions: impl Upcast<WcList>,
    goal: impl Upcast<WcData>,
) -> Set<ConstraintSet> {
    ProveWc(program.upcast(), assumptions.upcast(), goal.upcast()).apply()
}

#[term]
struct ProveWc(Program, WcList, WcData);

judgment! {
    (ProveWc => ConstraintSet)

    (
        (prove_apr(program, assumptions, a) => c)
        ---
        (ProveWc(program, assumptions, WcData::Atomic(a)) => c)
    )

    (
        (let p1 = binder.instantiate_universally(&assumptions))
        (prove_wc(program, assumptions, p1) => c)
        ---
        (ProveWc(program, assumptions, WcData::ForAll(binder)) => c)
    )

    (
        (prove_wc(program, assumptions.and(p1), p2) => c)
        ---
        (ProveWc(program, assumptions, WcData::Implies(p1, p2)) => c)
    )
}
