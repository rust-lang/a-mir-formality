use std::sync::Arc;

use crate::{
    from_into_term::Upcast,
    grammar::{Lt, LtData, Parameter, ParameterKind, Ty, TyData, Variable},
};

/// Invoked for each variable that we find when folding, ignoring variables bound by binders
/// that we traverse. The arguments are as follows:
///
/// * ParameterKind -- the kind of term in which the variable appeared (type vs lifetime, etc)
/// * Variable -- the variable we encountered
pub type SubstitutionFn<'a> = &'a mut dyn FnMut(ParameterKind, &Variable) -> Option<Parameter>;

pub trait Fold: Sized {
    /// Replace uses of variables with values from the substitution.
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self;

    /// Extract the list of free variables (for the purposes of this function, defined by `Variable::is_free`).
    fn free_variables(&self) -> Vec<Variable>;

    /// Produce a version of this term where any debruijn indices which appear free are incremented by one.
    fn shift_in(&self) -> Self {
        self.substitute(&mut |kind, v| Some(v.shift_in().into_parameter(kind)))
    }
}

impl<T: Fold> Fold for Vec<T> {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
        self.iter().map(|e| e.substitute(substitution_fn)).collect()
    }

    fn free_variables(&self) -> Vec<Variable> {
        self.iter().flat_map(|e| e.free_variables()).collect()
    }
}

impl<T: Fold> Fold for Option<T> {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
        self.as_ref().map(|e| e.substitute(substitution_fn))
    }

    fn free_variables(&self) -> Vec<Variable> {
        self.iter().flat_map(|e| e.free_variables()).collect()
    }
}

impl<T: Fold> Fold for Arc<T> {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
        let data = T::substitute(self, substitution_fn);
        Arc::new(data)
    }

    fn free_variables(&self) -> Vec<Variable> {
        T::free_variables(self)
    }
}

impl Fold for Ty {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
        match self.data() {
            TyData::RigidTy(v) => v.substitute(substitution_fn).upcast(),
            TyData::AliasTy(v) => v.substitute(substitution_fn).upcast(),
            TyData::PredicateTy(v) => v.substitute(substitution_fn).upcast(),
            TyData::Variable(v) => match substitution_fn(ParameterKind::Ty, v) {
                None => self.clone(),
                Some(Parameter::Ty(t)) => t,
                Some(param) => panic!("ill-kinded substitute: expected type, got {param:?}"),
            },
        }
    }

    fn free_variables(&self) -> Vec<Variable> {
        match self.data() {
            TyData::RigidTy(v) => v.free_variables(),
            TyData::AliasTy(v) => v.free_variables(),
            TyData::PredicateTy(v) => v.free_variables(),
            TyData::Variable(v) => {
                if v.is_free() {
                    vec![v.clone()]
                } else {
                    vec![]
                }
            }
        }
    }
}

impl Fold for Lt {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
        match self.data() {
            LtData::Static => self.clone(),
            LtData::Variable(v) => match substitution_fn(ParameterKind::Lt, v) {
                None => self.clone(),
                Some(Parameter::Lt(t)) => t,
                Some(param) => panic!("ill-kinded substitute: expected lifetime, got {param:?}"),
            },
        }
    }

    fn free_variables(&self) -> Vec<Variable> {
        match self.data() {
            LtData::Variable(v) => {
                if v.is_free() {
                    vec![v.clone()]
                } else {
                    vec![]
                }
            }
            LtData::Static => vec![],
        }
    }
}

impl Fold for usize {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_>) -> Self {
        *self
    }

    fn free_variables(&self) -> Vec<Variable> {
        vec![]
    }
}

impl Fold for u32 {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_>) -> Self {
        *self
    }

    fn free_variables(&self) -> Vec<Variable> {
        vec![]
    }
}

impl<A: Fold, B: Fold> Fold for (A, B) {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
        let (a, b) = self;
        (a.substitute(substitution_fn), b.substitute(substitution_fn))
    }

    fn free_variables(&self) -> Vec<Variable> {
        let (a, b) = self;
        let mut fv = vec![];
        fv.extend(a.free_variables());
        fv.extend(b.free_variables());
        fv
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

    fn free_variables(&self) -> Vec<Variable> {
        let (a, b, c) = self;
        let mut fv = vec![];
        fv.extend(a.free_variables());
        fv.extend(b.free_variables());
        fv.extend(c.free_variables());
        fv
    }
}
