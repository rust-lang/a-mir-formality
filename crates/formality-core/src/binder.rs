//! Manages binders so that the main rules can be nice and simple.

use std::sync::atomic::{AtomicUsize, Ordering};

use anyhow::bail;
use lazy_static::lazy_static;

use crate::{
    cast::{Downcast, DowncastFrom, DowncastTo, To, Upcast, UpcastFrom},
    fold::Fold,
    fold::SubstitutionFn,
    language::{HasKind, Kind, Language, Parameter},
    substitution::Substitution,
    variable::{BoundVar, DebruijnIndex, VarIndex, Variable},
    visit::Visit,
    Fallible,
};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Binder<L: Language, T> {
    kinds: Vec<Kind<L>>,
    term: T,
}

impl<L: Language, T: Fold<L>> Binder<L, T> {
    /// Accesses the contents of the binder.
    ///
    /// The variables inside will be renamed to fresh var indices
    /// that do not alias any other indices seen during this computation.
    ///
    /// The expectation is that you will create a term and use `Binder::new`.
    pub fn open(&self) -> (Vec<BoundVar<L>>, T) {
        let (bound_vars, substitution): (Vec<BoundVar<L>>, Substitution<L>) = self
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
                (new_bound_var, (old_bound_var, new_bound_var))
            })
            .unzip();

        (bound_vars, substitution.apply(&self.term))
    }

    pub fn dummy(term: T) -> Self {
        let v: Vec<Variable<L>> = vec![];
        Self::new(v, term)
    }

    /// Given a set of variables (X, Y, Z) and a term referecing some subset of them,
    /// create a binder where exactly those variables are bound (even the ones not used).
    pub fn new(variables: impl Upcast<Vec<Variable<L>>>, term: T) -> Self {
        let variables: Vec<Variable<L>> = variables.upcast();
        let (kinds, substitution): (Vec<Kind<L>>, Substitution<L>) = variables
            .iter()
            .zip(0..)
            .map(|(old_bound_var, index)| {
                let old_bound_var: Variable<L> = old_bound_var.upcast();
                assert!(old_bound_var.is_free());
                let new_bound_var: Parameter<L> = BoundVar {
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

    /// Given a set of variables (X, Y, Z) and a term referecing some subset of them,
    /// create a binder for just those variables that are mentioned.
    pub fn mentioned(variables: impl Upcast<Vec<Variable<L>>>, term: T) -> Self {
        let mut variables: Vec<Variable<L>> = variables.upcast();
        let fv = term.free_variables();
        variables.retain(|v| fv.contains(v));
        let variables: Vec<Variable<L>> = variables.into_iter().collect();
        Binder::new(variables, term)
    }

    pub fn into<U>(self) -> Binder<L, U>
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

    pub fn is_empty(&self) -> bool {
        self.kinds.is_empty()
    }

    /// Instantiate the binder with the given parameters, returning an err if the parameters
    /// are the wrong number or ill-kinded.
    pub fn instantiate_with(&self, parameters: &[impl Upcast<Parameter<L>>]) -> Fallible<T> {
        if parameters.len() != self.kinds.len() {
            bail!("wrong number of parameters");
        }

        for ((p, k), i) in parameters.iter().zip(&self.kinds).zip(0..) {
            let p: Parameter<L> = p.upcast();
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
    pub fn instantiate(&self, mut op: impl FnMut(Kind<L>, VarIndex) -> Parameter<L>) -> T {
        let substitution: Vec<Parameter<L>> = self
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
            }) => Some(substitution[var_index.index].clone()),

            _ => None,
        })
    }

    /// Accesses the data inside the binder. Use this for simple tests that extract data
    /// that is independent of the bound variables. If that's not the case, use `open`.
    pub fn peek(&self) -> &T {
        &self.term
    }

    /// Returns the kinds of each variable bound by this binder
    pub fn kinds(&self) -> &[Kind<L>] {
        &self.kinds
    }

    pub fn map<U: Fold<L>>(&self, op: impl FnOnce(T) -> U) -> Binder<L, U> {
        let (vars, t) = self.open();
        let u = op(t);
        Binder::new(vars, u)
    }
}

/// Creates a fresh bound var of the given kind that is not yet part of a binder.
/// You can put this into a term and then use `Binder::new`.
pub fn fresh_bound_var<L: Language>(kind: Kind<L>) -> BoundVar<L> {
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

impl<L: Language, T: Visit<L>> Visit<L> for Binder<L, T> {
    fn free_variables(&self) -> Vec<Variable<L>> {
        self.term.free_variables()
    }

    fn size(&self) -> usize {
        self.term.size()
    }

    fn assert_valid(&self) {
        self.term.assert_valid();
    }
}

impl<L: Language, T: Fold<L>> Fold<L> for Binder<L, T> {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Self {
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

impl<L: Language, T, U> UpcastFrom<Binder<L, T>> for Binder<L, U>
where
    T: Clone,
    U: Clone,
    T: Upcast<U>,
{
    fn upcast_from(term: Binder<L, T>) -> Self {
        let Binder { kinds, term } = term;
        Binder {
            kinds,
            term: term.upcast(),
        }
    }
}

impl<L: Language, T, U> DowncastTo<Binder<L, T>> for Binder<L, U>
where
    T: DowncastFrom<U>,
{
    fn downcast_to(&self) -> Option<Binder<L, T>> {
        let Binder { kinds, term } = self;
        let term = term.downcast()?;
        Some(Binder {
            kinds: kinds.clone(),
            term,
        })
    }
}

impl<L: Language, T> std::fmt::Debug for Binder<L, T>
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
