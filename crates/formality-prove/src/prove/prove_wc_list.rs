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
            (if let None = wcs.split_first())
            --- ("none")
            (JudgmentStruct(_env, _assumptions, wcs) => ConstraintSet::new())
        )

        (
            (if let Some((wc0, wcs1)) = wcs.split_first())
            (prove_wc(&program, &assumptions, wc0) => c1)
            (prove_after(&program, &assumptions, c1, &wcs1) => c2)
            --- ("some")
            (JudgmentStruct(program, assumptions, wcs) => c2)
        )
    }
}
