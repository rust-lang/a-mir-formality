use formality_types::{
    grammar::{Binder, Wcs},
    judgment_fn,
};

use crate::{
    program::Program,
    prove::{constraints::merge_constraints, prove, prove_wc_list::prove_wc_list},
};

use super::{constraints::Constraints, subst::existential_substitution};

judgment_fn! {
    pub fn prove_after(
        program: Program,
        constraints_in: Binder<Constraints>,
        assumptions: Wcs,
        goal: Wcs,
    ) => Binder<Constraints> {
        (
            (let existentials = existential_substitution(&c1, (&assumptions, &goal)))
            (let c1 = c1.instantiate_with(&existentials).unwrap())
            (let assumptions = c1.substitution().apply(&assumptions))
            (let goal = c1.substitution().apply(&goal))
            (prove(program, assumptions, goal) => c2)
            --- ("prove_after")
            (prove_after(program, c1, assumptions, goal) => merge_constraints(&existentials, &c1, c2))
        )
    }
}
