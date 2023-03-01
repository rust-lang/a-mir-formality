use formality_macros::term;
use formality_types::{
    cast::Upcast,
    collections::{Set, SetExt},
    fold::Fold,
    grammar::{AtomicRelation, Variable, Wcs},
    judgment_fn, set,
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
    let e: ConstraintSet = set![];
    prove_after_with_vars(program, assumptions, constraints_in, goal, e)
}

#[term]
struct ProveAfterWithVars(Program, Wcs, ConstraintSet, Wcs, ConstraintSet);

judgment_fn! {
    fn prove_after_with_vars(
        program: Program,
        wcs: Wcs,
        constraints_in: ConstraintSet,
        goal: Wcs,
        constraints_v: ConstraintSet
    ) => ConstraintSet {
        (
            (if let Some((AtomicRelation::Equals(p1, p2), constraints_in_1)) = constraints_in.split_first())
            (if let Some(Variable::InferenceVar(v)) = p1.as_variable())
            // XXX occurs and universe check
            (let assumptions = assumptions.replace_free_var(v, &p2))
            (let constraints_in_1 = constraints_in_1.replace_free_var(v, &p2))
            (let constraints_v = constraints_v.replace_free_var(v, &p2))
            (let goal = goal.replace_free_var(v, &p2))
            (prove_after_with_vars(program, assumptions, constraints_in_1, goal, constraints_v.plus(eq(v, p2))) => c)
            --- ("unify-l")
            (prove_after_with_vars(program, assumptions, constraints_in, goal, constraints_v) => c)
        )

        (
            (if let Some((AtomicRelation::Equals(p1, p2), constraints_in_1)) = constraints_in.split_first())
            (if let None = p1.as_variable())
            (if let Some(_) = p2.as_variable())
            (prove_after_with_vars(program, assumptions, set![..constraints_in_1, eq(p2, p1)], goal, constraints_v) => c)
            --- ("unify-r")
            (prove_after_with_vars(program, assumptions, constraints_in, goal, constraints_v) => c)
        )

        (
            (if let Some((AtomicRelation::Equals(p1, p2), constraints_in_1)) = constraints_in.split_first())
            (if let None = p1.as_variable())
            (if let None = p2.as_variable())
            (prove_after_with_vars(program, assumptions, constraints_in_1, all![goals, eq(p1, p2)], constraints_v) => c)
            --- ("prove")
            (prove_after_with_vars(program, assumptions, constraints_in, goals, constraints_v) => c)
        )

        (
            (if let None = constraints_in.split_first())
            (prove_wc_list(program, assumptions, goal) => c)
            --- ("true")
            (prove_after_with_vars(program, assumptions, constraints_in, goal, constraints_v) => constraints_v.clone().union_with(c))
        )
    }
}
