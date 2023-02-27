use formality_macros::term;
use formality_types::{
    cast::Upcast,
    collections::Set,
    grammar::{WcList, WcListData},
    judgment,
    judgment::Judgment,
};

use crate::{program::Program, prove::prove_after::prove_after};

use super::{prove_wc::prove_wc, ConstraintSet};

pub fn prove_wc_list(
    program: impl Upcast<Program>,
    assumptions: impl Upcast<WcList>,
    goal: impl Upcast<WcListData>,
) -> Set<ConstraintSet> {
    ProveWcListData(program.upcast(), assumptions.upcast(), goal.upcast()).apply()
}

#[term]
struct ProveWcListData(Program, WcList, WcListData);

judgment! {
    (ProveWcListData => ConstraintSet)

    (
        ---
        (ProveWcListData(_env, _assumptions, WcListData::True) => ConstraintSet::new())
    )

    (
        (prove_wc(program, assumptions, wc) => c)
        ---
        (ProveWcListData(program, assumptions, WcListData::Wc(wc)) => c)
    )

    (
        (prove_wc_list(&program, assumptions.clone(), l1) => c1)
        (prove_after(&program, &assumptions, c1, &l2) => c2)
        ---
        (ProveWcListData(program, assumptions, WcListData::And(l1, l2)) => c2)
    )
}
