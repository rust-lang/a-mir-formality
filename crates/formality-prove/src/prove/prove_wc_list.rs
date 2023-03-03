use formality_types::{
    grammar::{Binder, Wcs},
    judgment_fn,
};

use crate::{
    program::Program,
    prove::{
        constraints::{no_constraints, Constraints},
        prove_after::prove_after,
    },
};

use super::prove_wc::prove_wc;

judgment_fn! {
    pub fn prove_wc_list(
        program: Program,
        assumptions: Wcs,
        goal: Wcs,
    ) => Binder<Constraints> {
        (
            --- ("none")
            (prove_wc_list(_env, _assumptions, ()) => no_constraints())
        )

        (
            (prove_wc(&program, &assumptions, wc0) => c1)
            (prove_after(&program, c1, &assumptions, &wcs1) => c2)
            --- ("some")
            (prove_wc_list(program, assumptions, (wc0, wcs1)) => c2)
        )
    }
}
