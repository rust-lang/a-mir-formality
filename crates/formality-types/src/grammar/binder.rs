//! Manages binders so that the main rules can be nice and simple.

use std::sync::atomic::{AtomicUsize, Ordering};

use anyhow::bail;
use lazy_static::lazy_static;

use crate::{
    cast::{Downcast, DowncastFrom, DowncastTo, To, Upcast, UpcastFrom},
    fold::Fold,
    fold::SubstitutionFn,
    grammar::VarIndex,
    visit::Visit,
};

use super::{BoundVar, DebruijnIndex, Fallible, Parameter, ParameterKind, Substitution, Variable};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    pub fn open(&self) -> (Vec<BoundVar>, T) {
        let (bound_vars, substitution): (Vec<BoundVar>, Substitution) = self
            .kinds
            .iter()
            .zip(0..)
            .map(|(kind, index)| {
                let old_bound_var = BoundVar {
                    debruijn: Some(DebruijnIndex::INNERMOST),
                    var_index: VarIndex { index },
                    kind: *kind,
                };
                let new_bound_var = fresh_bound_var(*kind);
                (
                    new_bound_var,
                    (old_bound_var.upcast(), new_bound_var.upcast()),
                )
            })
            .unzip();

        (bound_vars, substitution.apply(&self.term))
    }

    pub fn dummy(term: T) -> Self {
        let v: Vec<Variable> = vec![];
        Self::new(&v, term)
    }

    /// Given a set of variables (X, Y, Z) and a term referecing them,
    /// create a binder where those variables are bound.
    pub fn new(variables: &[impl Upcast<Variable>], term: T) -> Self {
        let (kinds, substitution): (Vec<ParameterKind>, Substitution) = variables
            .iter()
            .zip(0..)
            .map(|(old_bound_var, index)| {
                let old_bound_var: Variable = old_bound_var.upcast();
                assert!(old_bound_var.is_free());
                let new_bound_var: Parameter = BoundVar {
                    debruijn: Some(DebruijnIndex::INNERMOST),
                    var_index: VarIndex { index },
                    kind: old_bound_var.kind(),
                }
                .upcast();
                (old_bound_var.kind(), (old_bound_var, new_bound_var))
            })
            .unzip();

        let term = substitution.apply(&term);
        Binder { kinds, term }
    }

    pub fn into<U>(self) -> Binder<U>
    where
        T: Into<U>,
    {
        Binder {
            kinds: self.kinds,
            term: self.term.into(),
        }
    }

    /// Number of variables bound by this binder
    pub fn len(&self) -> usize {
        self.kinds.len()
    }

    /// Instantiate the binder with the given parameters, returning an err if the parameters
    /// are the wrong number or ill-kinded.
    pub fn instantiate_with(&self, parameters: &[impl Upcast<Parameter>]) -> Fallible<T> {
        if parameters.len() != self.kinds.len() {
            bail!("wrong number of parameters");
        }

        for ((p, k), i) in parameters.iter().zip(&self.kinds).zip(0..) {
            let p: Parameter = p.upcast();
            if p.kind() != *k {
                bail!(
                    "parameter {i} has kind {:?} but should have kind {:?}",
                    p.kind(),
                    k
                );
            }
        }

        Ok(self.instantiate(|_kind, index| parameters[index.index].to()))
    }

    /// Instantiate the term, replacing each bound variable with `op(i)`.
    pub fn instantiate(&self, mut op: impl FnMut(ParameterKind, VarIndex) -> Parameter) -> T {
        let substitution: Vec<Parameter> = self
            .kinds
            .iter()
            .zip(0..)
            .map(|(&kind, index)| op(kind, VarIndex { index }))
            .collect();

        self.term.substitute(&mut |var| match var {
            Variable::BoundVar(BoundVar {
                debruijn: Some(DebruijnIndex::INNERMOST),
                var_index,
                kind: _,
            }) => Some(substitution[var_index.index as usize].clone()),

            _ => None,
        })
    }

    /// Accesses the data inside the binder. Use this for simple tests that extract data
    /// that is independent of the bound variables. If that's not the case, use `open`.
    pub fn peek(&self) -> &T {
        &self.term
    }

    /// Returns the kinds of each variable bound by this binder
    pub fn kinds(&self) -> &[ParameterKind] {
        &self.kinds
    }
}

/// Creates a fresh bound var of the given kind that is not yet part of a binder.
/// You can put this into a term and then use `Binder::new`.
pub fn fresh_bound_var(kind: ParameterKind) -> BoundVar {
    lazy_static! {
        static ref COUNTER: AtomicUsize = AtomicUsize::new(0);
    }

    let index = COUNTER.fetch_add(1, Ordering::SeqCst);
    let var_index = VarIndex { index };
    BoundVar {
        debruijn: None,
        var_index,
        kind,
    }
}

impl<T: Visit> Visit for Binder<T> {
    fn free_variables(&self) -> Vec<Variable> {
        self.term.free_variables()
    }
}

impl<T: Fold> Fold for Binder<T> {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
        let term = self.term.substitute(&mut |v| {
            // Shift this variable out through the binder. If that fails,
            // it's a variable bound by this binder, so the substitution can't
            // affect it, and we can just return None.
            let v1 = v.shift_out()?;

            // Get the result of the subst (if any).
            let parameter = substitution_fn(v1)?;

            // Shift that result in to account for this binder.
            Some(parameter.shift_in())
        });

        Binder {
            kinds: self.kinds.clone(),
            term,
        }
    }

    fn shift_in(&self) -> Self {
        let term = self.term.shift_in();
        Binder {
            kinds: self.kinds.clone(),
            term,
        }
    }
}

impl<T, U> UpcastFrom<Binder<T>> for Binder<U>
where
    T: Clone,
    U: Clone,
    T: Upcast<U>,
{
    fn upcast_from(term: Binder<T>) -> Self {
        let Binder { kinds, term } = term;
        Binder {
            kinds,
            term: term.upcast(),
        }
    }
}

impl<T, U> DowncastTo<Binder<T>> for Binder<U>
where
    T: DowncastFrom<U>,
{
    fn downcast_to(&self) -> Option<Binder<T>> {
        let Binder { kinds, term } = self;
        let term = term.downcast()?;
        Some(Binder {
            kinds: kinds.clone(),
            term,
        })
    }
}

impl<T> std::fmt::Debug for Binder<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<")?;
        for (kind, i) in self.kinds.iter().zip(0..) {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}", kind)?;
        }
        write!(f, "> ")?;
        write!(f, "{:?}", &self.term)?;
        Ok(())
    }
}
