use formality_types::{grammar::Wcs, judgment_fn};

use crate::{
    program::Program,
    prove::{env::Env, prove},
};

use super::constraints::Constraints;

judgment_fn! {
    pub fn prove_after(
        program: Program,
        env1: Env,
        c1: Constraints,
        assumptions: Wcs,
        goal: Wcs,
    ) => (Env, Constraints) {
        (
            (let (assumptions, goal) = c1.substitution().apply(&(assumptions, goal)))
            (prove(program, env1, assumptions, goal) => (env2, c2))
            --- ("prove_after")
            (prove_after(program, env1, c1, assumptions, goal) => (env2, c1.seq(c2)))
        )
    }
}
