use crate::prove::Bias;
use formality_core::ProvenSet;
use formality_macros::term;
use formality_types::grammar::{Binder, Wcs};
use std::sync::Arc;

use crate::{
    decls::Decls,
    prove::{prove, Constraints, Env},
};

/// Useful assertions for use in tests.
#[term]
pub enum TestAssertionPart {
    #[grammar(forall $v0)]
    ForAll(Binder<Arc<TestAssertionPart>>),
    #[grammar(exists $v0)]
    Exists(Binder<Arc<TestAssertionPart>>),
    #[grammar($v0 => $v1)]
    Prove(Wcs, Wcs),
}

#[term]
pub enum TestAssertion {
    #[cast]
    Default(Arc<TestAssertionPart>),
    #[grammar(coherence_mode $v0)]
    CoherenceMode(Arc<TestAssertionPart>),
}

/// `t` represents some set of existential bindings combined with (assumptions, goals).
/// Returns the constraints that result from proving assumptions/goals. These will reference
/// existential variables created for the bindings, so they're really just suitable for
/// using with expect.
pub fn test_prove(decls: Decls, assertion: Arc<TestAssertion>) -> ProvenSet<Constraints> {
    let (mut assertion, bias) = match &*assertion {
        TestAssertion::Default(assertion) => (assertion.clone(), Bias::Soundness),
        TestAssertion::CoherenceMode(assertion) => (assertion.clone(), Bias::Completeness),
    };

    let mut env = Env::new_with_bias(bias);

    loop {
        match &*assertion {
            TestAssertionPart::ForAll(binder) => {
                let (env1, subst) = env.universal_substitution(binder);
                let assertion1 = binder.instantiate_with(&subst).unwrap();
                env = env1;
                assertion = assertion1;
            }

            TestAssertionPart::Exists(binder) => {
                let (env1, subst) = env.existential_substitution(binder);
                let assertion1 = binder.instantiate_with(&subst).unwrap();
                env = env1;
                assertion = assertion1;
            }

            TestAssertionPart::Prove(assumptions, goals) => {
                return prove(decls, env, assumptions, goals);
            }
        }
    }
}
