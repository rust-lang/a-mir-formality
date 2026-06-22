use crate::grammar::Wcs;
use formality_core::{judgment_fn, Cons};

use crate::prove::prove::decls::Program;

use super::{env::Env, prove_wc::prove_wc};
use crate::prove::prove::prove;

judgment_fn! {
    /// Prove that all elements in `goals`, a list of where-clauses, are true, one after the other.
    pub fn prove_wc_list(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        goals: Wcs,
    ) => Env {
        debug(goals, assumptions, env)

        assert(env.encloses((assumptions, goals)))

        (
            --- ("none")
            (prove_wc_list(_decls, env, _assumptions, ()) => env)
        )

        (
            (prove_wc(decls, env, assumptions, wc0) => env)
            (prove(decls, env, assumptions, wcs1) => env)
            --- ("some")
            (prove_wc_list(decls, env, assumptions, Cons(wc0, wcs1)) => env)
        )
    }
}
