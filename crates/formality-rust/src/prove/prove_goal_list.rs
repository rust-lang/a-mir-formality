use crate::grammar::Goals;
use formality_core::{judgment_fn, Cons};

use crate::prove::{constraints::Constraints, decls::Program, prove_after::prove_after};

use super::{env::Env, prove_goal::prove_goal};

judgment_fn! {
    /// Prove that every goal in `goals` is true, one after the other.
    pub fn prove_goal_list(
        _decls: Program,
        env: Env,
        assumptions: Goals,
        goals: Goals,
    ) => Constraints {
        debug(goals, assumptions, env)

        assert(env.encloses((assumptions, goals)))

        (
            --- ("none")
            (prove_goal_list(_decls, env, _assumptions, ()) => Constraints::none(env))
        )

        (
            (prove_goal(decls, env, assumptions, goal0) => c)
            (prove_after(decls, c, assumptions, goals1) => c)
            --- ("some")
            (prove_goal_list(decls, env, assumptions, Cons(goal0, goals1)) => c)
        )
    }
}
