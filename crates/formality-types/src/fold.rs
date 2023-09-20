use std::sync::Arc;

use crate::{
    cast::Upcast,
    collections::Set,
    grammar::{Const, ConstData, Lt, LtData, Parameter, Ty, TyData, ValTree, Variable},
    visit::Visit,
};

/// Invoked for each variable that we find when folding, ignoring variables bound by binders
/// that we traverse. The arguments are as follows:
///
/// * ParameterKind -- the kind of term in which the variable appeared (type vs lifetime, etc)
/// * Variable -- the variable we encountered
pub type SubstitutionFn<'a> = &'a mut dyn FnMut(Variable) -> Option<Parameter>;

pub trait Fold: Sized + Visit {
    /// Replace uses of variables with values from the substitution.
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self;

    /// Produce a version of this term where any debruijn indices which appear free are incremented by one.
    fn shift_in(&self) -> Self {
        self.substitute(&mut |v| Some(v.shift_in().upcast()))
    }

    /// Replace all appearances of free variable `v` with `p`.
    fn replace_free_var(&self, v: impl Upcast<Variable>, p: impl Upcast<Parameter>) -> Self {
        let v: Variable = v.upcast();
        let p: Parameter = p.upcast();
        assert!(v.is_free());
        assert!(v.kind() == p.kind());
        self.substitute(&mut |v1| if v == v1 { Some(p.clone()) } else { None })
    }
}

impl<T: Fold> Fold for Vec<T> {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
        self.iter().map(|e| e.substitute(substitution_fn)).collect()
    }
}

impl<T: Fold + Ord> Fold for Set<T> {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
        self.iter().map(|e| e.substitute(substitution_fn)).collect()
    }
}

impl<T: Fold> Fold for Option<T> {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
        self.as_ref().map(|e| e.substitute(substitution_fn))
    }
}

impl<T: Fold> Fold for Arc<T> {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
        let data = T::substitute(self, substitution_fn);
        Arc::new(data)
    }
}

impl Fold for Ty {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
        match self.data() {
            TyData::RigidTy(v) => v.substitute(substitution_fn).upcast(),
            TyData::AliasTy(v) => v.substitute(substitution_fn).upcast(),
            TyData::PredicateTy(v) => v.substitute(substitution_fn).upcast(),
            TyData::Variable(v) => match substitution_fn(*v) {
                None => self.clone(),
                Some(Parameter::Ty(t)) => t,
                Some(param) => panic!("ill-kinded substitute: expected type, got {param:?}"),
            },
        }
    }
}

impl Fold for Const {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
        match self.data() {
            ConstData::Value(v, ty) => Self::valtree(
                v.substitute(substitution_fn),
                ty.substitute(substitution_fn),
            ),
            ConstData::Variable(v) => match substitution_fn(*v) {
                None => self.clone(),
                Some(Parameter::Const(c)) => c,
                Some(param) => panic!("ill-kinded substitute: expected const, got {param:?}"),
            },
        }
    }
}

impl Fold for ValTree {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_>) -> Self {
        self.clone()
    }
}

impl Fold for Lt {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
        match self.data() {
            LtData::Static => self.clone(),
            LtData::Variable(v) => match substitution_fn(*v) {
                None => self.clone(),
                Some(Parameter::Lt(t)) => t,
                Some(param) => panic!("ill-kinded substitute: expected lifetime, got {param:?}"),
            },
        }
    }
}

impl Fold for usize {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_>) -> Self {
        *self
    }
}

impl Fold for u32 {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_>) -> Self {
        *self
    }
}

impl Fold for () {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_>) -> Self {}
}

impl<A: Fold, B: Fold> Fold for (A, B) {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
        let (a, b) = self;
        (a.substitute(substitution_fn), b.substitute(substitution_fn))
    }
}

impl<A: Fold, B: Fold, C: Fold> Fold for (A, B, C) {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
        let (a, b, c) = self;
        (
            a.substitute(substitution_fn),
            b.substitute(substitution_fn),
            c.substitute(substitution_fn),
        )
    }
}
