use formality_macros::term;
use formality_types::{
    cast::Upcast,
    collections::Set,
    grammar::{WcData, WcList, APR},
    judgment,
    judgment::Judgment,
};

use crate::{
    program::Program,
    prove::{prove_after::prove_after, prove_eq::prove_parameters_eq},
};

use super::ConstraintSet;

pub fn prove_apr_via(
    env: impl Upcast<Program>,
    assumptions: impl Upcast<WcList>,
    via: impl Upcast<WcData>,
    goal: impl Upcast<APR>,
) -> Set<ConstraintSet> {
    ProveAprVia(
        env.upcast(),
        assumptions.upcast(),
        via.upcast(),
        goal.upcast(),
    )
    .apply()
}

#[term]
struct ProveAprVia(Program, WcList, WcData, APR);

judgment! {
    (ProveAprVia => ConstraintSet)

    (
        (let (skel_c, parameters_c) = clause.debone())
        (let (skel_g, parameters_g) = goal.debone())
        (if skel_c == skel_g)
        (prove_parameters_eq(env, assumptions, parameters_c, parameters_g) => c)
        -----------------------------
        (ProveAprVia(env, assumptions, WcData::Atomic(clause), goal) => c)
    )

    (
        (let via1 = env.instantiate_existentially(&assumptions, &binder))
        (prove_apr_via(env, assumptions, via1, goal) => c)
        -----------------------------
        (ProveAprVia(env, assumptions, WcData::ForAll(binder), goal) => c)
    )

    (
        (prove_apr_via(&env, &assumptions, wc_consequence, goal) => c1)
        (prove_after(&env, &assumptions, c1, &wc_condition) => c2)
        -----------------------------
        (ProveAprVia(env, assumptions, WcData::Implies(wc_condition, wc_consequence), goal) => c2)
    )
}
