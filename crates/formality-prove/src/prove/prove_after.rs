use formality_macros::term;
use formality_types::{
    cast::Upcast,
    collections::{Set, SetExt},
    fold::Fold,
    grammar::{AtomicRelation, Variable, Wcs},
    judgment,
    judgment::Judgment,
    set,
};

use crate::{
    program::Program,
    prove::{prove_eq::eq, prove_wc_list::prove_wc_list},
};

use super::ConstraintSet;

pub fn prove_after(
    program: impl Upcast<Program>,
    assumptions: impl Upcast<Wcs>,
    constraints_in: ConstraintSet,
    goal: impl Upcast<Wcs>,
) -> Set<ConstraintSet> {
    prove_after_with_vars(program, assumptions, constraints_in, goal, set![])
}

fn prove_after_with_vars(
    program: impl Upcast<Program>,
    assumptions: impl Upcast<Wcs>,
    constraints_in: ConstraintSet,
    goal: impl Upcast<Wcs>,
    constraints_v: ConstraintSet,
) -> Set<ConstraintSet> {
    ProveAfterWithVars(
        program.upcast(),
        assumptions.upcast(),
        constraints_in,
        goal.upcast(),
        constraints_v,
    )
    .apply()
}

#[term]
struct ProveAfterWithVars(Program, Wcs, ConstraintSet, Wcs, ConstraintSet);

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
        (prove_after_with_vars(program, assumptions, constraints_in_1, goal, set![AtomicRelation::eq(v, p2), ..constraints_v]) => c)
        ---
        (ProveAfterWithVars(program, assumptions, constraints_in, goal, constraints_v) => c)
    )

    (
        (if let Some((AtomicRelation::Equals(p1, p2), constraints_in_1)) = constraints_in.split_first())
        (if let None = p1.as_variable())
        (if let Some(_) = p2.as_variable())
        (prove_after_with_vars(program, assumptions, set![..constraints_in_1, AtomicRelation::Equals(p2, p1)], goal, constraints_v) => c)
        ---
        (ProveAfterWithVars(program, assumptions, constraints_in, goal, constraints_v) => c)
    )

    (
        (if let Some((AtomicRelation::Equals(p1, p2), constraints_in_1)) = constraints_in.split_first())
        (if let None = p1.as_variable())
        (if let None = p2.as_variable())
        (prove_after_with_vars(program, assumptions, constraints_in_1, set![..goals, eq(p1, p2)], constraints_v) => c)
        ---
        (ProveAfterWithVars(program, assumptions, constraints_in, goals, constraints_v) => c)
    )

    (
        (if let None = constraints_in.split_first())
        (prove_wc_list(program, assumptions, goal) => c)
        ---
        (ProveAfterWithVars(program, assumptions, constraints_in, goal, constraints_v) => constraints_v.union_with(c))
    )
}
