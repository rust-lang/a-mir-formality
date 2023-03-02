use formality_types::{
    grammar::{Wc, WcData, Wcs},
    judgment_fn,
    visit::Visit,
};

use crate::{
    program::Program,
    prove::{
        forall::constraints_visible_from_universe, prove_apr::prove_apr,
        subst::universal_substitution,
    },
};

use super::ConstraintSet;

judgment_fn! {
    pub fn prove_wc(
        program: Program,
        assumptions: Wcs,
        goal: Wc,
    ) => ConstraintSet {
        (
            (prove_apr(program, assumptions, a) => c)
            --- ("atomic")
            (prove_wc(program, assumptions, WcData::Atomic(a)) => c)
        )

        (
            (let subst = universal_substitution(&binder, &assumptions))
            (let p1 = binder.instantiate_with(&subst).unwrap())
            (prove_wc(program, &assumptions, p1) => c)
            (let u = (&binder, &assumptions).max_universe())
            (let c1 = constraints_visible_from_universe(u, c))
            --- ("forall")
            (prove_wc(program, assumptions, WcData::ForAll(binder)) => c1)
        )

        (
            (prove_wc(program, (assumptions, p1), p2) => c)
            --- ("implies")
            (prove_wc(program, assumptions, WcData::Implies(p1, p2)) => c)
        )
    }
}
