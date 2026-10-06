//! The scope of a binder's variables, for the `scope` condition of a rule:
//!
//! ```ignore
//! (scope(enter_universally_with_constraints(decls, env, assumptions, binder) => (env, body)) with(c)
//!     (prove_wc(decls, env, assumptions, body) => c))
//! ```
//!
//! Inside are the body of the binder, with a fresh variable for each of its
//! variables, and the environment with those in scope.
//!
//! A rule that takes constraints out of the binder enters it with
//! [`enter_universally_with_constraints`][] or
//! [`enter_existentially_with_constraints`][]: leaving may have to prove the
//! where-clauses that were pending on its variables, which needs the
//! declarations and the assumptions to prove them under. A rule that proves
//! nothing to take out enters with [`enter_universally`][] or
//! [`enter_existentially`][] and leaves with `with()`; it cannot pass an
//! assumption set that leaving would never use.

use crate::grammar::{Binder, Lt, Parameter, Predicate, Variable, Wc, Wcs};
use crate::prove::{constraints::Constraints, decls::Program, env::Env, prove_after::prove_after};
use crate::rust::Term;
use formality_core::judgment::{FailureLocation, ProofTree, Scope};
use formality_core::visit::CoreVisit;
use formality_core::{Downcast, ProvenSet, Upcast};

/// The scope of a binder's variables, entered with [`enter_universally`][]
/// or [`enter_existentially`][]. Nothing that mentions those variables
/// leaves it: see [`BinderScopeWithConstraints`][] for a scope that
/// constraints leave.
pub struct BinderScope {
    /// The fresh variables, in scope inside.
    vars: Vec<Variable>,
}

/// A [`BinderScope`][] that constraints leave, entered with
/// [`enter_universally_with_constraints`][] or
/// [`enter_existentially_with_constraints`][]. They leave without the
/// binder's variables: those are popped, and a where-clause still pending
/// on one is restated without it, then proven or deferred again outside --
/// which is what the declarations and assumptions are for.
pub struct BinderScopeWithConstraints {
    scope: BinderScope,
    decls: Program,
    assumptions: Wcs,
}

/// Enter `binder` with fresh universal variables: what is proven inside
/// holds for every value of the binder's variables.
pub fn enter_universally<T: Term>(
    env: impl Upcast<Env>,
    binder: &Binder<T>,
) -> (BinderScope, (Env, T)) {
    let (env, vars) = env.upcast().universal_substitution(binder);
    let body = binder.instantiate_with(&vars).unwrap();
    (
        BinderScope {
            vars: vars.upcast(),
        },
        (env, body),
    )
}

/// Enter `binder` with fresh existential variables: what is proven inside
/// holds for some value of the binder's variables.
pub fn enter_existentially<T: Term>(
    env: impl Upcast<Env>,
    binder: &Binder<T>,
) -> (BinderScope, (Env, T)) {
    let (env, vars) = env.upcast().existential_substitution(binder);
    let body = binder.instantiate_with(&vars).unwrap();
    (
        BinderScope {
            vars: vars.upcast(),
        },
        (env, body),
    )
}

/// [`enter_universally`][], for a rule that takes constraints out of the
/// binder.
pub fn enter_universally_with_constraints<T: Term>(
    decls: impl Upcast<Program>,
    env: impl Upcast<Env>,
    assumptions: impl Upcast<Wcs>,
    binder: &Binder<T>,
) -> (BinderScopeWithConstraints, (Env, T)) {
    let (scope, inside) = enter_universally(env, binder);
    (scope.with_constraints(decls, assumptions), inside)
}

/// [`enter_existentially`][], for a rule that takes constraints out of the
/// binder.
pub fn enter_existentially_with_constraints<T: Term>(
    decls: impl Upcast<Program>,
    env: impl Upcast<Env>,
    assumptions: impl Upcast<Wcs>,
    binder: &Binder<T>,
) -> (BinderScopeWithConstraints, (Env, T)) {
    let (scope, inside) = enter_existentially(env, binder);
    (scope.with_constraints(decls, assumptions), inside)
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

impl Scope<(Constraints,)> for BinderScopeWithConstraints {
    fn leave(&self, (c,): (Constraints,)) -> ProvenSet<(Constraints,)> {
        self.pop(c).map(|(c, proof_tree)| ((c,), proof_tree))
    }
}

/// A value proven along with the constraints must not mention the variables.
impl<V: Term> Scope<(V, Constraints)> for BinderScopeWithConstraints {
    fn leave(&self, (value, c): (V, Constraints)) -> ProvenSet<(V, Constraints)> {
        self.pop(c).map(|(c, proof_tree)| {
            assert!(c.env().encloses(&value));
            ((value.clone(), c), proof_tree)
        })
    }
}

impl BinderScope {
    /// The fresh variables, in scope inside.
    pub fn vars(&self) -> &[Variable] {
        &self.vars
    }

    /// This scope, for a rule that takes constraints out of the binder.
    fn with_constraints(
        self,
        decls: impl Upcast<Program>,
        assumptions: impl Upcast<Wcs>,
    ) -> BinderScopeWithConstraints {
        BinderScopeWithConstraints {
            scope: self,
            decls: decls.upcast(),
            assumptions: assumptions.upcast(),
        }
    }
}

impl BinderScopeWithConstraints {
    /// `c` without the scope's variables, and any created since, in its
    /// environment and substitution.
    ///
    /// A where-clause still pending may mention one of them. It cannot stay
    /// pending as is: whoever discharges it later has no such variable in
    /// scope. So it is restated without the variable (`without_var`), then
    /// proven, or deferred again, outside.
    fn pop(&self, mut c: Constraints) -> ProvenSet<Constraints> {
        let popped = c.env.variables_since(self.scope.vars());

        // The substitution may bind a variable the where-clauses mention.
        let pending = c.substitution.apply(&c.env.take_pending_on(&popped));
        c.substitution -= c.env.pop_vars(self.scope.vars());
        c.assert_valid();

        if pending.is_empty() {
            return ProvenSet::singleton((c, ProofTree::leaf("nothing pending on the variables")));
        }

        // Innermost first: a variable may depend on those created before it.
        let mut pending: Wcs = pending.into_iter().collect();
        for &v in popped.iter().rev() {
            pending = match without_var(&pending, v) {
                Ok(pending) => pending,
                Err(wc) => {
                    return ProvenSet::failed(
                        "leave binder",
                        FailureLocation::caller(),
                        format!("`{wc:?}` cannot be restated without `{v:?}`"),
                    )
                }
            };
        }
        prove_after(&self.decls, c, &self.assumptions, pending)
    }
}

/// `pending` restated without the variable `v`, or the where-clause that
/// cannot be.
///
/// | `v`              | pending        | restated     |                                     |
/// |------------------|----------------|--------------|-------------------------------------|
/// | universal `!a`   | `X: !a`        | `X: 'static` | `!a` may be `'static`               |
/// | universal `!a`   | `!a: Y`        | cannot be    | `!a` may be shorter than `Y`        |
/// | existential `?a` | `X: ?a, ?a: Y` | `X: Y`       | some `?a` lies between iff `X: Y`   |
/// | either           | `&a u32: Y`    | cannot be    | only a bare `a` on one side is read |
///
/// The last row is why a clause that merely *mentions* `v` is an error
/// rather than a dropped obligation: restating it would need the outlives
/// relation of the type that contains `v`, which this does not compute.
pub fn without_var(pending: &Wcs, v: Variable) -> Result<Wcs, Wc> {
    let mut restated: Vec<Wc> = vec![];
    let mut outlive_v: Vec<Parameter> = vec![];
    let mut outlived_by_v: Vec<Parameter> = vec![];

    for wc in pending {
        match &wc {
            _ if !wc.free_variables().contains(&v) => restated.push(wc),
            Wc::Predicate(Predicate::Outlives(x, b))
                if b.downcast() == Some(v) && !x.free_variables().contains(&v) =>
            {
                outlive_v.push(x.clone())
            }
            Wc::Predicate(Predicate::Outlives(a, y))
                if v.is_existential()
                    && a.downcast() == Some(v)
                    && !y.free_variables().contains(&v) =>
            {
                outlived_by_v.push(y.clone())
            }
            _ => return Err(wc),
        }
    }

    if v.is_universal() {
        outlived_by_v.push(Lt::Static.upcast());
    }
    for x in &outlive_v {
        for y in &outlived_by_v {
            restated.push(Predicate::outlives(x, y).upcast());
        }
    }
    Ok(restated.into_iter().collect())
}

#[cfg(test)]
mod test {
    use super::without_var;
    use crate::grammar::{
        Lt, ParameterKind, Predicate, ScalarId, Ty, UniversalVar, VarIndex, Variable, Wcs,
    };
    use expect_test::expect;
    use formality_core::Upcast;
    use formality_macros::test;

    /// A pending where-clause whose left side is a type *mentioning* the
    /// variable cannot be restated: the table reads a bare `v` on one side,
    /// not a `v` buried in `X`. Such a clause is an error rather than a
    /// dropped obligation.
    #[test]
    fn type_mentioning_variable() {
        let v: Variable = UniversalVar {
            kind: ParameterKind::Lt,
            var_index: VarIndex::ZERO,
        }
        .upcast();
        // `&v u32 : 'static`, which mentions `v` inside the reference type.
        let pending: Wcs =
            Predicate::outlives(Ty::rigid(ScalarId::U32, ()).ref_ty(v), Lt::static_()).upcast();

        expect!["cannot restate `&!lt_0 u32 : \' static`"].assert_eq(
            &match without_var(&pending, v) {
                Ok(restated) => format!("restated as `{restated:?}`"),
                Err(wc) => format!("cannot restate `{wc:?}`"),
            },
        );
    }
}
