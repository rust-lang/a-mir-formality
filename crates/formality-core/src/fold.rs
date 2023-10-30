use std::sync::Arc;

use crate::{
    cast::Upcast,
    collections::Set,
    language::{HasKind, Language, Parameter},
    variable::Variable,
    visit::Visit,
};

/// Invoked for each variable that we find when folding, ignoring variables bound by binders
/// that we traverse. The arguments are as follows:
///
/// * ParameterKind -- the kind of term in which the variable appeared (type vs lifetime, etc)
/// * Variable -- the variable we encountered
#[allow(type_alias_bounds)]
pub type SubstitutionFn<'a, L: Language> = &'a mut dyn FnMut(Variable<L>) -> Option<Parameter<L>>;

pub trait Fold<L: Language>: Sized + Visit<L> {
    /// Replace uses of variables with values from the substitution.
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Self;

    /// Produce a version of this term where any debruijn indices which appear free are incremented by one.
    fn shift_in(&self) -> Self {
        self.substitute(&mut |v| Some(v.shift_in().upcast()))
    }

    /// Replace all appearances of free variable `v` with `p`.
    fn replace_free_var(&self, v: impl Upcast<Variable<L>>, p: impl Upcast<Parameter<L>>) -> Self {
        let v: Variable<L> = v.upcast();
        let p: Parameter<L> = p.upcast();
        assert!(v.is_free());
        assert!(v.kind() == p.kind());
        self.substitute(&mut |v1| if v == v1 { Some(p.clone()) } else { None })
    }
}

impl<L: Language, T: Fold<L>> Fold<L> for Vec<T> {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Self {
        self.iter().map(|e| e.substitute(substitution_fn)).collect()
    }
}

impl<L: Language, T: Fold<L> + Ord> Fold<L> for Set<T> {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Self {
        self.iter().map(|e| e.substitute(substitution_fn)).collect()
    }
}

impl<L: Language, T: Fold<L>> Fold<L> for Option<T> {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Self {
        self.as_ref().map(|e| e.substitute(substitution_fn))
    }
}

impl<L: Language, T: Fold<L>> Fold<L> for Arc<T> {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Self {
        let data = T::substitute(self, substitution_fn);
        Arc::new(data)
    }
}

impl<L: Language> Fold<L> for usize {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> Fold<L> for u32 {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> Fold<L> for () {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {}
}

impl<L: Language, A: Fold<L>, B: Fold<L>> Fold<L> for (A, B) {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Self {
        let (a, b) = self;
        (a.substitute(substitution_fn), b.substitute(substitution_fn))
    }
}

impl<L: Language, A: Fold<L>, B: Fold<L>, C: Fold<L>> Fold<L> for (A, B, C) {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Self {
        let (a, b, c) = self;
        (
            a.substitute(substitution_fn),
            b.substitute(substitution_fn),
            c.substitute(substitution_fn),
        )
    }
}
