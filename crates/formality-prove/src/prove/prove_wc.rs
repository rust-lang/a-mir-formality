use formality_macros::term;
use formality_types::{
    cast::Upcast,
    collections::Set,
    grammar::{WcData, WcList},
    judgment,
    judgment::Judgment,
};

use crate::{env::Env, prove::prove_apr::prove_apr};

use super::ConstraintSet;

pub fn prove_wc(
    env: impl Upcast<Env>,
    assumptions: impl Upcast<WcList>,
    goal: impl Upcast<WcData>,
) -> Set<ConstraintSet> {
    ProveWc(env.upcast(), assumptions.upcast(), goal.upcast()).apply()
}

#[term]
struct ProveWc(Env, WcList, WcData);

judgment! {
    (ProveWc => ConstraintSet)

    (
        (prove_apr(env, assumptions, a) => c)
        ---
        (ProveWc(env, assumptions, WcData::Atomic(a)) => c)
    )

    (
        (let (p1, env1) = env.instantiate_universally(&binder))
        (prove_wc(env1, assumptions, p1) => c)
        ---
        (ProveWc(env, assumptions, WcData::ForAll(binder)) => c)
    )

    (
        (prove_wc(env, assumptions.and(p1), p2) => c)
        ---
        (ProveWc(env, assumptions, WcData::Implies(p1, p2)) => c)
    )
}
