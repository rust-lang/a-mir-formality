use formality_types::{
    grammar::{Wc, WcData, Wcs},
    judgment_fn,
};

use crate::{
    program::Program,
    prove::{env::Env, prove_apr::prove_apr},
};

use super::constraints::Constraints;

judgment_fn! {
    pub fn prove_wc(
        program: Program,
        env: Env,
        assumptions: Wcs,
        goal: Wc,
    ) => (Env, Constraints) {
        (
            (prove_apr(program, env, assumptions, a) => c)
            --- ("atomic")
            (prove_wc(program, env, assumptions, WcData::Atomic(a)) => c)
        )

        (
            (let (env1, subst) = env.universal_substitution(&binder))
            (let p1 = binder.instantiate_with(&subst).unwrap())
            (prove_wc(program, env1, &assumptions, p1) => (env2, c2))
            (let (env3, c3) = c2.pop_subst(env2, &subst))
            --- ("forall")
            (prove_wc(program, env, assumptions, WcData::ForAll(binder)) => (env3, c3))
        )

        (
            (prove_wc(program, env, (assumptions, p1), p2) => c)
            --- ("implies")
            (prove_wc(program, env, assumptions, WcData::Implies(p1, p2)) => c)
        )
    }
}
