use formality_types::{
    grammar::{Wc, WcData, Wcs},
    judgment_fn,
};

use crate::{
    decls::Decls,
    prove::{env::Env, prove_pr::prove_pr},
};

use super::constraints::Constraints;

judgment_fn! {
    pub fn prove_wc(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        goal: Wc,
    ) => Constraints {
        debug(goal, assumptions, env, decls)

        (
            (prove_pr(decls, env, assumptions, a) => c)
            --- ("atomic")
            (prove_wc(decls, env, assumptions, WcData::PR(a)) => c)
        )

        (
            (let (env, subst) = env.universal_substitution(&binder))
            (let p1 = binder.instantiate_with(&subst).unwrap())
            (prove_wc(decls, env, &assumptions, p1) => c)
            --- ("forall")
            (prove_wc(decls, env, assumptions, WcData::ForAll(binder)) => c.pop_subst(&subst))
        )

        (
            (prove_wc(decls, env, (assumptions, p1), p2) => c)
            --- ("implies")
            (prove_wc(decls, env, assumptions, WcData::Implies(p1, p2)) => c)
        )
    }
}
