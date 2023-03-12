use std::sync::Arc;

use formality_macros::term;
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
mod magic_copy;
mod occurs_check;
mod simple_impl;
mod universes;

#[term]
enum TestAssertion {
    #[grammar(forall $v0)]
    ForAll(Binder<Arc<TestAssertion>>),
    #[grammar(exists $v0)]
    Exists(Binder<Arc<TestAssertion>>),
    #[grammar($v0 => $v1)]
    Prove(Wcs, Wcs),
}

/// `t` represents some set of existential bindings combined with (assumptions, goals).
/// Returns the constraints that result from proving assumptions/goals. These will reference
/// existential variables created for the bindings, so they're really just suitable for
/// using with expect.
fn test_prove(program: Program, mut assertion: Arc<TestAssertion>) -> Set<Constraints> {
    let mut env = Env::default();

    loop {
        match &*assertion {
            TestAssertion::ForAll(binder) => {
                let (env1, subst) = env.universal_substitution(binder);
                let assertion1 = binder.instantiate_with(&subst).unwrap();
                env = env1;
                assertion = assertion1;
            }

            TestAssertion::Exists(binder) => {
                let (env1, subst) = env.existential_substitution(binder);
                let assertion1 = binder.instantiate_with(&subst).unwrap();
                env = env1;
                assertion = assertion1;
            }

            TestAssertion::Prove(assumptions, goals) => {
                return prove(program, env, assumptions, goals);
            }
        }
    }
}
