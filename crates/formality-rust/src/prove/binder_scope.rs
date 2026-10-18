//! The scope of a binder's variables, for the `scope` condition of a rule:
//!
//! ```ignore
//! (scope(enter_universally_with_constraints(env, binder) => (env, body)) with(c)
//!     (prove_predicate(decls, env, assumptions, body) => c))
//! ```
//!
//! Inside are the body of the binder, with a fresh variable for each of its
//! variables, and the environment with those in scope.
//!
//! Which scope a rule enters says what may leave it. A rule that proves
//! nothing to take out enters a [`BinderScope`][] and leaves with `with()`;
//! one that takes constraints out enters a [`BinderScopeWithConstraints`][]
//! and leaves with `with(c)`. Neither implements the other's `leave`, so a
//! rule that pairs them the wrong way does not compile.

use crate::grammar::{Binder, Parameter, Variable};
use crate::prove::{constraints::Constraints, env::Env};
use crate::rust::Term;
use formality_core::judgment::{ProofTree, Scope};
use formality_core::{ProvenSet, Upcast};

/// The scope of a binder's variables, entered with [`enter_universally`][]
/// or [`enter_existentially`][]. Nothing that mentions those variables
/// leaves it: see [`BinderScopeWithConstraints`][] for a scope that
/// constraints leave.
pub struct BinderScope {
    /// The fresh variables, in scope inside.
    vars: Vec<Variable>,
}

/// The scope of a binder's variables that constraints leave, entered with
/// [`enter_universally_with_constraints`][] or
/// [`enter_existentially_with_constraints`][]. Leaving pops the variables
/// from the constraints.
pub struct BinderScopeWithConstraints {
    scope: BinderScope,
}

/// Enter `binder` with fresh universal variables: what is proven inside
/// holds for every value of the binder's variables.
pub fn enter_universally<T: Term>(
    env: impl Upcast<Env>,
    binder: &Binder<T>,
) -> (BinderScope, (Env, T)) {
    let (env, vars) = env.upcast().universal_substitution(binder);
    let body = binder.instantiate_with(&vars).unwrap();
    let scope = BinderScope {
        vars: vars.upcast(),
    };
    (scope, (env, body))
}

/// Enter `binder` with fresh existential variables: what is proven inside
/// holds for some value of the binder's variables.
pub fn enter_existentially<T: Term>(
    env: impl Upcast<Env>,
    binder: &Binder<T>,
) -> (BinderScope, (Env, T)) {
    let (env, vars) = env.upcast().existential_substitution(binder);
    let body = binder.instantiate_with(&vars).unwrap();
    let scope = BinderScope {
        vars: vars.upcast(),
    };
    (scope, (env, body))
}

/// [`enter_universally`][], for a rule that takes constraints out.
pub fn enter_universally_with_constraints<T: Term>(
    env: impl Upcast<Env>,
    binder: &Binder<T>,
) -> (BinderScopeWithConstraints, (Env, T)) {
    let (scope, inside) = enter_universally(env, binder);
    (scope.with_constraints(), inside)
}

/// [`enter_existentially`][], for a rule that takes constraints out.
pub fn enter_existentially_with_constraints<T: Term>(
    env: impl Upcast<Env>,
    binder: &Binder<T>,
) -> (BinderScopeWithConstraints, (Env, T)) {
    let (scope, inside) = enter_existentially(env, binder);
    (scope.with_constraints(), inside)
}

/// `binder` with its variables alongside its body, for a rule that needs
/// the fresh variables themselves.
pub fn with_variables<T: Term>(binder: &Binder<T>) -> Binder<(Vec<Parameter>, T)> {
    let (vars, body) = binder.open();
    let parameters = vars.iter().map(|v| v.upcast()).collect();
    Binder::new(&vars, (parameters, body))
}

/// Nothing proven inside is taken out.
impl Scope<()> for BinderScope {
    fn leave(&self, (): ()) -> ProvenSet<()> {
        ProvenSet::singleton(((), ProofTree::leaf("leave binder")))
    }
}

impl BinderScope {
    /// This scope, for a rule that takes constraints out of the binder.
    fn with_constraints(self) -> BinderScopeWithConstraints {
        BinderScopeWithConstraints { scope: self }
    }
}

impl Scope<(Constraints,)> for BinderScopeWithConstraints {
    fn leave(&self, (c,): (Constraints,)) -> ProvenSet<(Constraints,)> {
        ProvenSet::singleton(((self.pop(c),), ProofTree::leaf("leave binder")))
    }
}

/// A value proven along with the constraints must not mention the variables.
impl<V: Term> Scope<(V, Constraints)> for BinderScopeWithConstraints {
    fn leave(&self, (value, c): (V, Constraints)) -> ProvenSet<(V, Constraints)> {
        let c = self.pop(c);
        assert!(c.env().encloses(&value));
        ProvenSet::singleton(((value, c), ProofTree::leaf("leave binder")))
    }
}

impl BinderScopeWithConstraints {
    /// `c` without the scope's variables, and any created since, in its
    /// environment and substitution.
    fn pop(&self, mut c: Constraints) -> Constraints {
        c.substitution -= c.env.pop_vars(&self.scope.vars);
        c
    }
}
