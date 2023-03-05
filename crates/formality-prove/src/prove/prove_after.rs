use formality_types::{
    grammar::{Binder, Wcs},
    judgment_fn,
};

use crate::{
    program::Program,
    prove::{
        constraints::{instantiate_and_apply_constraints, merge_constraints},
        prove,
    },
};

use super::constraints::Constraints;

judgment_fn! {
    pub fn prove_after(
        program: Program,
        constraints_in: Binder<Constraints>,
        assumptions: Wcs,
        goal: Wcs,
    ) => Binder<Constraints> {
        (
            (let (existentials, c1, (assumptions, goal)) = instantiate_and_apply_constraints(c1, (assumptions, goal)))
            (prove(program, assumptions, goal) => c2)
            --- ("prove_after")
            (prove_after(program, c1, assumptions, goal) => merge_constraints(&existentials, &c1, c2))
        )
    }
}
