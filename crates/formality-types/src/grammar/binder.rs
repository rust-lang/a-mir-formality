//! Manages binders so that the main rules can be nice and simple.

use std::sync::atomic::{AtomicU64, Ordering};

use lazy_static::lazy_static;

use crate::{fold::Fold, grammar::VarIndex};

use super::{BoundVar, DebruijnIndex, KindedVarIndex, Parameter, ParameterKind, Substitution};

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
    /// The expectation is that you will
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

    /// Given a set of 
    pub fn new(variables: Vec<KindedVarIndex>, term: T) -> Self {
        let (kinds, substitution): (Vec<ParameterKind>, Substitution) = variables
            .iter()
            .zip(0..)
            .map(|(kinded_index, index)| {
                let old_bound_var = BoundVar {
                    debruijn: None,
                    var_index: kinded_index.var_index,
                };
                let new_bound_var = BoundVar {
                    debruijn: Some(DebruijnIndex::INNERMOST),
                    var_index: VarIndex { index },
                };
                (kinded_index.kind, (old_bound_var, new_bound_var))
            })
            .unzip();

        let term = term.shift_in(1);
        let term = term.substitute(&substitution);
        Binder { kinds, term }
    }
}

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
