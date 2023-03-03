use formality_types::{
    grammar::{Binder, Wc, WcData, Wcs},
    judgment_fn,
};

use crate::{
    program::Program,
    prove::{
        forall::constrains_placeholder_in_universe, prove_apr::prove_apr,
        subst::universal_substitution,
    },
};

use super::constraints::Constraints;

judgment_fn! {
    pub fn prove_wc(
        program: Program,
        assumptions: Wcs,
        goal: Wc,
    ) => Binder<Constraints> {
        (
            (prove_apr(program, assumptions, a) => c)
            --- ("atomic")
            (prove_wc(program, assumptions, WcData::Atomic(a)) => c)
        )

        (
            (let (u, subst) = universal_substitution(&binder, &assumptions))
            (let p1 = binder.instantiate_with(&subst).unwrap())
            (prove_wc(program, &assumptions, p1) => c)
            (if !constrains_placeholder_in_universe(u, &c))
            --- ("forall")
            (prove_wc(program, assumptions, WcData::ForAll(binder)) => c)
        )

        (
            (prove_wc(program, (assumptions, p1), p2) => c)
            --- ("implies")
            (prove_wc(program, assumptions, WcData::Implies(p1, p2)) => c)
        )
    }
}
