use formality_types::{grammar::Wcs, judgment_fn};

use crate::{program::Program, prove::prove_after::prove_after};

use super::{prove_wc::prove_wc, ConstraintSet};

judgment_fn! {
    pub fn prove_wc_list(
        program: Program,
        assumptions: Wcs,
        goal: Wcs,
    ) => ConstraintSet {
        (
            --- ("none")
            (prove_wc_list(_env, _assumptions, ()) => ConstraintSet::new())
        )

        (
            (prove_wc(&program, &assumptions, wc0) => c1)
            (prove_after(&program, &assumptions, c1, &wcs1) => c2)
            --- ("some")
            (prove_wc_list(program, assumptions, (wc0, wcs1)) => c2)
        )
    }
}
