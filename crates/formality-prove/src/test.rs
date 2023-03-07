use formality_types::{
    collections::Set,
    grammar::{Binder, Wcs},
};

use crate::{
    program::Program,
    prove::{prove, Constraints, Env},
};

mod eq_assumptions;
mod eq_partial_eq;
mod exists_constraints;
mod expanding;
mod occurs_check;
mod simple_impl;
mod universes;

/// `t` represents some set of existential bindings combined with (assumptions, goals).
/// Returns the constraints that result from proving assumptions/goals. These will reference
/// existential variables created for the bindings, so they're really just suitable for
/// using with expect.
fn test_prove(program: Program, t: Binder<(Wcs, Wcs)>) -> Set<(Env, Constraints)> {
    let env = Env::default();
    let (env, subst) = env.existential_substitution(&t);
    let (assumptions, goals) = t.instantiate_with(&subst).unwrap();
    prove(program, env, assumptions, goals)
}
