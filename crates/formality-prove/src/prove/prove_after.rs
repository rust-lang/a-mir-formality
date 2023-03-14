use formality_types::{grammar::Wcs, judgment_fn};

use crate::{decls::Decls, prove::prove};

use super::constraints::Constraints;

judgment_fn! {
    pub fn prove_after(
        decls: Decls,
        constraints: Constraints,
        assumptions: Wcs,
        goal: Wcs,
    ) => Constraints {
        debug(constraints, goal, assumptions, decls)

        (
            (let (assumptions, goal) = c1.substitution().apply(&(assumptions, goal)))
            (prove(decls, c1.env(), assumptions, goal) => c2)
            --- ("prove_after")
            (prove_after(decls, c1, assumptions, goal) => c1.seq(c2))
        )
    }
}
