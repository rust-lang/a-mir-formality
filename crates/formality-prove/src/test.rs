use formality_types::{
    cast::Upcast,
    collections::Set,
    grammar::{Binder, InferenceVar, Wcs},
};

use crate::{
    program::Program,
    prove::{prove, Constraints},
};

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
fn test_prove(program: Program, t: Binder<(Wcs, Wcs)>) -> Set<Binder<Constraints>> {
    let (assumptions, goals) =
        t.instantiate(|kind, var_index| InferenceVar { kind, var_index }.upcast());
    prove(program, assumptions, goals)
}
