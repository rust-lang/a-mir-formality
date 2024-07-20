use formality_core::{judgment_fn, Cons};
use formality_types::grammar::Wcs;

use crate::{
    decls::Decls,
    prove::{constraints::Constraints, prove_after::prove_after},
};

use super::{env::Env, prove_wc::prove_wc};

judgment_fn! {
    pub fn prove_wc_list(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        goal: Wcs,
    ) => Constraints {
        debug(goal, assumptions, env)

        assert(env.encloses((&assumptions, &goal)))

        (
            --- ("none")
            (prove_wc_list(_decls, env, _assumptions, ()) => Constraints::none(env))
        )

        (
            (prove_wc(&decls, env, &assumptions, wc0) => c)
            (prove_after(&decls, c, &assumptions, &wcs1) => c)
            --- ("some")
            (prove_wc_list(decls, env, assumptions, Cons(wc0, wcs1)) => c)
        )
    }
}
