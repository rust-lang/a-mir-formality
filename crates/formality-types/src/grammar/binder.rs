//! Manages binders so that the main rules can be nice and simple.

use std::sync::atomic::{AtomicU64, Ordering};

use lazy_static::lazy_static;

use crate::{fold::Fold, fold::SubstitutionFn, grammar::VarIndex};

use super::{
    BoundVar, DebruijnIndex, KindedVarIndex, Parameter, ParameterKind, Substitution, Variable,
};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Binder<T> {
    kinds: Vec<ParameterKind>,
    term: T,
}

impl<T: Fold> Binder<T> {
    /// Accesses the contents of the binder.
    ///
    /// The variables inside will be renamed to fresh var indices
    /// that do not alias any other indices seen during this computation.
    ///
    /// The expectation is that you will create a term and use `Binder::new`.
    pub fn open(&self) -> (Vec<KindedVarIndex>, T) {
        let (var_kinds, substitution): (Vec<KindedVarIndex>, Substitution) = self
            .kinds
            .iter()
            .zip(0..)
            .map(|(kind, index)| {
                let old_bound_var = BoundVar {
                    debruijn: Some(DebruijnIndex::INNERMOST),
                    var_index: VarIndex { index },
                };
                let (kind_index, new_bound_var) = fresh_bound_var(*kind);
                (
                    kind_index,
                    (
                        old_bound_var.into(),
                        new_bound_var.into_parameter(kind_index.kind),
                    ),
                )
            })
            .unzip();

        (var_kinds, substitution.apply(&self.term))
    }

    /// Given a set of variables (X, Y, Z) and a term referecing them,
    /// create a binder where those variables are bound.
    pub fn new(variables: Vec<KindedVarIndex>, term: T) -> Self {
        let (kinds, substitution): (Vec<ParameterKind>, Substitution) = variables
            .iter()
            .zip(0..)
            .map(|(kinded_index, index)| {
                let old_bound_var: Variable = BoundVar {
                    debruijn: None,
                    var_index: kinded_index.var_index,
                }
                .into();
                let new_bound_var: Parameter = BoundVar {
                    debruijn: Some(DebruijnIndex::INNERMOST),
                    var_index: VarIndex { index },
                }
                .into_parameter(kinded_index.kind);
                (kinded_index.kind, (old_bound_var, new_bound_var))
            })
            .unzip();

        let term = substitution.apply(&term);
        Binder { kinds, term }
    }
}

/// Creates a fresh bound var of the given kind that is not yet part of a binder.
/// You can put this into a term and then use `Binder::new`.
pub fn fresh_bound_var(kind: ParameterKind) -> (KindedVarIndex, BoundVar) {
    lazy_static! {
        static ref COUNTER: AtomicU64 = AtomicU64::new(0);
    }

    let index = COUNTER.fetch_add(1, Ordering::SeqCst);
    let var_index = VarIndex { index };
    (
        KindedVarIndex { kind, var_index },
        BoundVar {
            debruijn: None,
            var_index,
        },
    )
}

impl<T: Fold> Fold for Binder<T> {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
        let term = self.term.substitute(&mut |kind, v| {
            // Shift this variable out through the binder. If that fails,
            // it's a variable bound by this binder, so the substitution can't
            // affect it, and we can just return None.
            let v1 = v.shift_out()?;

            // Get the result of the subst (if any).
            let parameter = substitution_fn(kind, v)?;

            // Shift that result in to account for this binder.
            Some(parameter.shift_in())
        });

        Binder {
            kinds: self.kinds.clone(),
            term,
        }
    }

    fn free_variables(&self) -> Vec<Variable> {
        self.term.free_variables()
    }

    fn shift_in(&self) -> Self {
        let term = self.term.shift_in();
        Binder {
            kinds: self.kinds.clone(),
            term,
        }
    }
}
