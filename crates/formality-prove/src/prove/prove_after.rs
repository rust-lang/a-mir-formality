use formality_macros::term;
use formality_types::{
    cast::Upcast,
    collections::{Set, SetExt},
    fold::Fold,
    grammar::{AtomicRelation, Variable, WcList},
    judgment,
    judgment::Judgment,
    set,
};

use crate::{program::Program, prove::prove_wc_list::prove_wc_list};

use super::ConstraintSet;

pub fn prove_after(
    env: impl Upcast<Program>,
    assumptions: impl Upcast<WcList>,
    constraints_in: ConstraintSet,
    goal: impl Upcast<WcList>,
) -> Set<ConstraintSet> {
    prove_after_with_vars(env, assumptions, constraints_in, goal, set![])
}

fn prove_after_with_vars(
    env: impl Upcast<Program>,
    assumptions: impl Upcast<WcList>,
    constraints_in: ConstraintSet,
    goal: impl Upcast<WcList>,
    constraints_v: ConstraintSet,
) -> Set<ConstraintSet> {
    ProveAfterWithVars(
        env.upcast(),
        assumptions.upcast(),
        constraints_in,
        goal.upcast(),
        constraints_v,
    )
    .apply()
}

#[term]
struct ProveAfterWithVars(Program, WcList, ConstraintSet, WcList, ConstraintSet);

judgment! {
    (ProveAfterWithVars => ConstraintSet)

    (
        (if let Some((AtomicRelation::Equals(p1, p2), constraints_in_1)) = constraints_in.split_first())
        (if let Some(Variable::InferenceVar(v)) = p1.as_variable())
        // XXX occurs and universe check
        (let assumptions = assumptions.replace_free_var(v, &p2))
        (let constraints_in_1 = constraints_in_1.replace_free_var(v, &p2))
        (let constraints_v = constraints_v.replace_free_var(v, &p2))
        (let goal = goal.replace_free_var(v, &p2))
        (prove_after_with_vars(env, assumptions, constraints_in_1, goal, set![AtomicRelation::eq(v, p2), ..constraints_v]) => c)
        ---
        (ProveAfterWithVars(env, assumptions, constraints_in, goal, constraints_v) => c)
    )

    (
        (if let Some((AtomicRelation::Equals(p1, p2), constraints_in_1)) = constraints_in.split_first())
        (if let None = p1.as_variable())
        (if let Some(_) = p2.as_variable())
        (prove_after_with_vars(env, assumptions, set![..constraints_in_1, AtomicRelation::Equals(p2, p1)], goal, constraints_v) => c)
        ---
        (ProveAfterWithVars(env, assumptions, constraints_in, goal, constraints_v) => c)
    )

    (
        (if let Some((AtomicRelation::Equals(p1, p2), constraints_in_1)) = constraints_in.split_first())
        (if let None = p1.as_variable())
        (if let None = p2.as_variable())
        (prove_after_with_vars(env, assumptions, constraints_in_1, goal.and(AtomicRelation::Equals(p1, p2)), constraints_v) => c)
        ---
        (ProveAfterWithVars(env, assumptions, constraints_in, goal, constraints_v) => c)
    )

    (
        (if let None = constraints_in.split_first())
        (prove_wc_list(env, assumptions, goal) => c)
        ---
        (ProveAfterWithVars(env, assumptions, constraints_in, goal, constraints_v) => constraints_v.union_with(c))
    )
}
