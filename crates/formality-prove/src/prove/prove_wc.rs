
use formality_types::{
    cast::Upcast,
    grammar::{WcData, Wcs},
    judgment_fn,
    visit::Visit,
};

use crate::{
    program::Program,
    prove::{forall::constraints_visible_from_universe, prove_apr::prove_apr},
};

use super::ConstraintSet;

judgment_fn! {
    pub fn prove_wc(
        program: Program,
        assumptions: Wcs,
        goal: WcData,
    ) => ConstraintSet {
        (
            (prove_apr(program, assumptions, a) => c)
            --- ("atomic")
            (JudgmentStruct(program, assumptions, WcData::Atomic(a)) => c)
        )

        (
            (let u = assumptions.max_universe())
            (let p1 = binder.instantiate_universally(&assumptions))
            (prove_wc(program, assumptions, p1) => c)
            (let c1 = constraints_visible_from_universe(u, c))
            --- ("forall")
            (JudgmentStruct(program, assumptions, WcData::ForAll(binder)) => c1)
        )

        (
            (prove_wc(program, all![assumptions, p1], p2) => c)
            --- ("implies")
            (JudgmentStruct(program, assumptions, WcData::Implies(p1, p2)) => c)
        )
    }
}
