use formality_types::{grammar::Wcs, judgment_fn};

use crate::{program::Program, prove::prove};

use super::constraints::Constraints;

judgment_fn! {
    pub fn prove_after(
        program: Program,
        constraints: Constraints,
        assumptions: Wcs,
        goal: Wcs,
    ) => Constraints {
        (
            (let (assumptions, goal) = c1.substitution().apply(&(assumptions, goal)))
            (prove(program, c1.env(), assumptions, goal) => c2)
            --- ("prove_after")
            (prove_after(program, c1, assumptions, goal) => c1.seq(c2))
        )
    }
}
