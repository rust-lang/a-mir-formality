use formality_types::{grammar::Wcs, judgment_fn};

use crate::{
    program::Program,
    prove::{constraints::Constraints, prove_after::prove_after},
};

use super::{env::Env, prove_wc::prove_wc};

judgment_fn! {
    pub fn prove_wc_list(
        program: Program,
        env: Env,
        assumptions: Wcs,
        goal: Wcs,
    ) => Constraints {
        debug(goal, assumptions, env, program)

        (
            --- ("none")
            (prove_wc_list(_program, env, _assumptions, ()) => Constraints::none(env))
        )

        (
            (prove_wc(&program, env, &assumptions, wc0) => c)
            (prove_after(&program, c, &assumptions, &wcs1) => c)
            --- ("some")
            (prove_wc_list(program, env, assumptions, (wc0, wcs1)) => c)
        )
    }
}
