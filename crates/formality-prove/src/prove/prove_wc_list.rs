use formality_macros::term;
use formality_types::{
    cast::Upcast,
    collections::Set,
    grammar::{WcList, WcListData},
    judgment,
    judgment::Judgment,
};

use crate::{env::Env, prove::prove_after::prove_after};

use super::{prove_wc::prove_wc, ConstraintSet};

pub fn prove_wc_list(
    env: Env,
    assumptions: impl Upcast<WcList>,
    goal: impl Upcast<WcListData>,
) -> Set<ConstraintSet> {
    ProveWcListData(env, assumptions.upcast(), goal.upcast()).apply()
}

#[term]
struct ProveWcListData(Env, WcList, WcListData);

judgment! {
    (ProveWcListData => ConstraintSet)

    (
        ---
        (ProveWcListData(_env, _assumptions, WcListData::True) => ConstraintSet::new())
    )

    (
        (prove_wc(env, assumptions, wc) => c)
        ---
        (ProveWcListData(env, assumptions, WcListData::Wc(wc)) => c)
    )

    (
        (prove_wc_list(env, assumptions.clone(), l1) => c1)
        (prove_after(env, &assumptions, c1, &l2) => c2)
        ---
        (ProveWcListData(env, assumptions, WcListData::And(l1, l2)) => c2)
    )
}
