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
    ) => Constraints {
        (
            (prove_apr(program, env, assumptions, a) => c)
            --- ("atomic")
            (prove_wc(program, env, assumptions, WcData::Atomic(a)) => c)
        )

        (
            (let (env, subst) = env.universal_substitution(&binder))
            (let p1 = binder.instantiate_with(&subst).unwrap())
            (prove_wc(program, env, &assumptions, p1) => c)
            --- ("forall")
            (prove_wc(program, env, assumptions, WcData::ForAll(binder)) => c.pop_subst(&subst))
        )

        (
            (prove_wc(program, env, (assumptions, p1), p2) => c)
            --- ("implies")
            (prove_wc(program, env, assumptions, WcData::Implies(p1, p2)) => c)
        )
    }
}
