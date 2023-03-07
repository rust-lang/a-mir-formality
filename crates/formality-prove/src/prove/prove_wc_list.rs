use formality_types::{grammar::Wcs, judgment_fn};

use crate::{
    program::Program,
    prove::{
        constraints::{no_constraints, Constraints},
        prove_after::prove_after,
    },
};

use super::{env::Env, prove_wc::prove_wc};

judgment_fn! {
    pub fn prove_wc_list(
        program: Program,
        env: Env,
        assumptions: Wcs,
        goal: Wcs,
    ) => (Env, Constraints) {
        (
            --- ("none")
            (prove_wc_list(_env, variables, _assumptions, ()) => (variables, no_constraints()))
        )

        (
            (prove_wc(&program, env0, &assumptions, wc0) => (env1, c1))
            (prove_after(&program, env1, c1, &assumptions, &wcs1) => c2)
            --- ("some")
            (prove_wc_list(program, env0, assumptions, (wc0, wcs1)) => c2)
        )
    }
}
