use formality_types::{grammar::Wcs, judgment_fn};

use crate::{
    program::Program,
    prove::{env::Env, prove},
};

use super::constraints::Constraints;

judgment_fn! {
    pub fn prove_after(
        program: Program,
        env: Env,
        constraints: Constraints,
        assumptions: Wcs,
        goal: Wcs,
    ) => (Env, Constraints) {
        (
            (let (assumptions, goal) = c1.substitution().apply(&(assumptions, goal)))
            (prove(program, env, assumptions, goal) => (env, c2))
            --- ("prove_after")
            (prove_after(program, env, c1, assumptions, goal) => (env, c1.seq(c2)))
        )
    }
}
