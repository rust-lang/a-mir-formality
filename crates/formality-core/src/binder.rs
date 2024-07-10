//! Manages binders so that the main rules can be nice and simple.

use std::sync::atomic::{AtomicUsize, Ordering};

use anyhow::bail;
use lazy_static::lazy_static;

use crate::{
    cast::{Downcast, DowncastFrom, DowncastTo, To, Upcast, UpcastFrom},
    fold::CoreFold,
    fold::SubstitutionFn,
    language::{CoreKind, CoreParameter, HasKind, Language},
    substitution::CoreSubstitution,
    variable::{CoreBoundVar, CoreVariable, DebruijnIndex, VarIndex},
    visit::CoreVisit,
    Fallible,
};

pub mod fuzz;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct CoreBinder<L: Language, T> {
    kinds: Vec<CoreKind<L>>,
    term: T,
}

impl<L: Language, T: CoreFold<L>> CoreBinder<L, T> {
    /// Accesses the contents of the binder.
    ///
    /// The variables inside will be renamed to fresh var indices
    /// that do not alias any other indices seen during this computation.
    ///
    /// The expectation is that you will create a term and use `Binder::new`.
    pub fn open(&self) -> (Vec<CoreBoundVar<L>>, T) {
        let (bound_vars, substitution): (Vec<CoreBoundVar<L>>, CoreSubstitution<L>) = self
            .kinds
            .iter()
            .zip(0..)
            .map(|(kind, index)| {
                let old_bound_var = CoreBoundVar {
                    debruijn: Some(DebruijnIndex::INNERMOST),
                    var_index: VarIndex { index },
                    kind: *kind,
                };
                let new_bound_var = CoreBoundVar::fresh(*kind);
                (new_bound_var, (old_bound_var, new_bound_var))
            })
            .unzip();

        (bound_vars, substitution.apply(&self.term))
    }

    pub fn dummy(term: T) -> Self {
        let v: Vec<CoreVariable<L>> = vec![];
        Self::new(v, term)
    }

    /// Given a set of variables (X, Y, Z) and a term referecing some subset of them,
    /// create a binder where exactly those variables are bound (even the ones not used).
    pub fn new(variables: impl Upcast<Vec<CoreVariable<L>>>, term: T) -> Self {
        let variables: Vec<CoreVariable<L>> = variables.upcast();
        let (kinds, substitution): (Vec<CoreKind<L>>, CoreSubstitution<L>) = variables
            .iter()
            .zip(0..)
            .map(|(old_bound_var, index)| {
                let old_bound_var: CoreVariable<L> = old_bound_var.upcast();
                assert!(old_bound_var.is_free());
                let new_bound_var: CoreParameter<L> = CoreBoundVar {
                    debruijn: Some(DebruijnIndex::INNERMOST),
                    var_index: VarIndex { index },
                    kind: old_bound_var.kind(),
                }
                .upcast();
                (old_bound_var.kind(), (old_bound_var, new_bound_var))
            })
            .unzip();

        let term = substitution.apply(&term);
        CoreBinder { kinds, term }
    }

    /// Given a set of variables (X, Y, Z) and a term referecing some subset of them,
    /// create a binder for just those variables that are mentioned.
    pub fn mentioned(variables: impl Upcast<Vec<CoreVariable<L>>>, term: T) -> Self {
        let mut variables: Vec<CoreVariable<L>> = variables.upcast();
        let fv = term.free_variables();
        variables.retain(|v| fv.contains(v));
        let variables: Vec<CoreVariable<L>> = variables.into_iter().collect();
        CoreBinder::new(variables, term)
    }

    pub fn into<U>(self) -> CoreBinder<L, U>
    where
        T: Into<U>,
    {
        CoreBinder {
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
    pub fn instantiate_with(&self, parameters: &[impl Upcast<CoreParameter<L>>]) -> Fallible<T> {
        if parameters.len() != self.kinds.len() {
            bail!("wrong number of parameters");
        }

        for ((p, k), i) in parameters.iter().zip(&self.kinds).zip(0..) {
            let p: CoreParameter<L> = p.upcast();
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
    pub fn instantiate(&self, mut op: impl FnMut(CoreKind<L>, VarIndex) -> CoreParameter<L>) -> T {
        let substitution: Vec<CoreParameter<L>> = self
            .kinds
            .iter()
            .zip(0..)
            .map(|(&kind, index)| op(kind, VarIndex { index }))
            .collect();

        self.term.substitute(&mut |var| match var {
            CoreVariable::BoundVar(CoreBoundVar {
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
    pub fn kinds(&self) -> &[CoreKind<L>] {
        &self.kinds
    }

    pub fn map<U: CoreFold<L>>(&self, op: impl FnOnce(T) -> U) -> CoreBinder<L, U> {
        let (vars, t) = self.open();
        let u = op(t);
        CoreBinder::new(vars, u)
    }
}

impl<L: Language> CoreBoundVar<L> {
    /// Creates a fresh bound var of the given kind that is not yet part of a binder.
    /// You can put this into a term and then use `Binder::new`.
    pub fn fresh(kind: CoreKind<L>) -> Self {
        lazy_static! {
            static ref COUNTER: AtomicUsize = AtomicUsize::new(0);
        }

        let index = COUNTER.fetch_add(1, Ordering::SeqCst);
        let var_index = VarIndex { index };
        CoreBoundVar {
            debruijn: None,
            var_index,
            kind,
        }
    }
}

impl<L: Language, T: CoreVisit<L>> CoreVisit<L> for CoreBinder<L, T> {
    fn free_variables(&self) -> Vec<CoreVariable<L>> {
        self.term.free_variables()
    }

    fn size(&self) -> usize {
        self.term.size()
    }

    fn assert_valid(&self) {
        self.term.assert_valid();
    }
}

impl<L: Language, T: CoreFold<L>> CoreFold<L> for CoreBinder<L, T> {
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

        CoreBinder {
            kinds: self.kinds.clone(),
            term,
        }
    }

    fn shift_in(&self) -> Self {
        let term = self.term.shift_in();
        CoreBinder {
            kinds: self.kinds.clone(),
            term,
        }
    }
}

impl<L: Language, T, U> UpcastFrom<CoreBinder<L, T>> for CoreBinder<L, U>
where
    T: Clone,
    U: Clone,
    T: Upcast<U>,
{
    fn upcast_from(term: CoreBinder<L, T>) -> Self {
        let CoreBinder { kinds, term } = term;
        CoreBinder {
            kinds,
            term: term.upcast(),
        }
    }
}

impl<L: Language, T, U> DowncastTo<CoreBinder<L, T>> for CoreBinder<L, U>
where
    T: DowncastFrom<U>,
{
    fn downcast_to(&self) -> Option<CoreBinder<L, T>> {
        let CoreBinder { kinds, term } = self;
        let term = term.downcast()?;
        Some(CoreBinder {
            kinds: kinds.clone(),
            term,
        })
    }
}

impl<L: Language, T> std::fmt::Debug for CoreBinder<L, T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            f.debug_struct("Binder")
                .field("kinds", &self.kinds)
                .field("term", &self.term)
                .finish()
        } else {
            if !self.kinds.is_empty() {
                write!(f, "{}", L::BINDING_OPEN)?;
                for (kind, i) in self.kinds.iter().zip(0..) {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", kind)?;
                }
                write!(f, "{} ", L::BINDING_CLOSE)?;
            }
            write!(f, "{:?}", &self.term)?;
            Ok(())
        }
    }
}

/// Creates a fresh bound var of the given kind that is not yet part of a binder.
/// You can put this into a term and then use `Binder::new`.
pub fn fresh_bound_var<L: Language>(kind: L::Kind) -> CoreBoundVar<L> {
    lazy_static! {
        static ref COUNTER: AtomicUsize = AtomicUsize::new(0);
    }

    let index = COUNTER.fetch_add(1, Ordering::SeqCst);
    let var_index = VarIndex { index };
    CoreBoundVar {
        debruijn: None,
        var_index,
        kind,
    }
}
