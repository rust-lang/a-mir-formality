use std::sync::Arc;

use formality_core::ProvenSet;
use formality_macros::term;
use formality_types::grammar::{Binder, Wcs};

use crate::{
    decls::Decls,
    prove::{prove, Constraints, Env},
};

/// Useful assertions for use in tests.
#[term]
pub enum TestAssertion {
    #[grammar(coherence_mode $v0)]
    CoherenceMode(Arc<TestAssertion>),
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
pub fn test_prove(decls: Decls, mut assertion: Arc<TestAssertion>) -> ProvenSet<Constraints> {
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
                return prove(decls, env, assumptions, goals);
            }

            TestAssertion::CoherenceMode(assertion1) => {
                env = env.with_coherence_mode(true);
                assertion = assertion1.clone();
            }
        }
    }
}
