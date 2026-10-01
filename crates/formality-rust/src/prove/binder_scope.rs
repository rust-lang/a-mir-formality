//! The scope of a binder's variables, for the `scope` condition of a rule:
//!
//! ```ignore
//! (scope(enter_universally(env, binder) => (env, body)) with(c)
//!     (prove_wc(decls, env, assumptions, body) => c))
//! ```
//!
//! Inside are the body of the binder, with a fresh variable for each of its
//! variables, and the environment with those in scope. The constraints `c`
//! come out without them: see [`BinderScope::pop`].

use crate::grammar::{Binder, Variable};
use crate::prove::{constraints::Constraints, env::Env};
use crate::rust::Term;
use formality_core::judgment::{ProofTree, Scope};
use formality_core::{ProvenSet, Upcast};

pub struct BinderScope {
    /// The fresh variables, in scope inside.
    vars: Vec<Variable>,
}

/// Enter `binder` with fresh universal variables: what is proven inside
/// holds for every value of the binder's variables.
pub fn enter_universally<T: Term>(env: &Env, binder: &Binder<T>) -> (BinderScope, (Env, T)) {
    let (env, vars) = env.universal_substitution(binder);
    let body = binder.instantiate_with(&vars).unwrap();
    let scope = BinderScope {
        vars: vars.upcast(),
    };
    (scope, (env, body))
}

/// Enter `binder` with fresh existential variables: what is proven inside
/// holds for some value of the binder's variables.
pub fn enter_existentially<T: Term>(env: &Env, binder: &Binder<T>) -> (BinderScope, (Env, T)) {
    let (env, vars) = env.existential_substitution(binder);
    let body = binder.instantiate_with(&vars).unwrap();
    let scope = BinderScope {
        vars: vars.upcast(),
    };
    (scope, (env, body))
}

impl Scope<(Constraints,)> for BinderScope {
    fn leave(&self, (c,): (Constraints,)) -> ProvenSet<(Constraints,)> {
        ProvenSet::singleton(((self.pop(c),), ProofTree::leaf("leave binder")))
    }
}

/// A value proven along with the constraints must not mention the variables.
impl<V: Term> Scope<(V, Constraints)> for BinderScope {
    fn leave(&self, (value, c): (V, Constraints)) -> ProvenSet<(V, Constraints)> {
        let c = self.pop(c);
        assert!(c.env().encloses(&value));
        ProvenSet::singleton(((value, c), ProofTree::leaf("leave binder")))
    }
}

impl BinderScope {
    /// `c` without the scope's variables, and any created since, in its
    /// environment and substitution.
    fn pop(&self, mut c: Constraints) -> Constraints {
        c.substitution -= c.env.pop_vars(&self.vars);
        c
    }
}
