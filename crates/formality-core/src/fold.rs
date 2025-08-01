use std::sync::Arc;

use crate::{
    cast::Upcast,
    collections::Set,
    language::{CoreParameter, HasKind, Language},
    variable::CoreVariable,
    visit::CoreVisit,
};

/// Invoked for each variable that we find when folding, ignoring variables bound by binders
/// that we traverse. The arguments are as follows:
///
/// * ParameterKind -- the kind of term in which the variable appeared (type vs lifetime, etc)
/// * Variable -- the variable we encountered
pub type SubstitutionFn<'a, L: Language> =
    &'a mut dyn FnMut(CoreVariable<L>) -> Option<CoreParameter<L>>;

pub trait CoreFold<L: Language>: Sized + CoreVisit<L> {
    /// Replace uses of variables with values from the substitution.
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Self;

    /// Produce a version of this term where any debruijn indices which appear free are incremented by one.
    fn shift_in(&self) -> Self {
        self.substitute(&mut |v| Some(v.shift_in().upcast()))
    }

    /// Replace all appearances of free variable `v` with `p`.
    fn replace_free_var(
        &self,
        v: impl Upcast<CoreVariable<L>>,
        p: impl Upcast<CoreParameter<L>>,
    ) -> Self {
        let v: CoreVariable<L> = v.upcast();
        let p: CoreParameter<L> = p.upcast();
        assert!(v.is_free());
        assert!(v.kind() == p.kind());
        self.substitute(&mut |v1| if v == v1 { Some(p.clone()) } else { None })
    }
}

impl<L: Language, T: CoreFold<L>> CoreFold<L> for Vec<T> {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Self {
        self.iter().map(|e| e.substitute(substitution_fn)).collect()
    }
}

impl<L: Language, T: CoreFold<L> + Ord> CoreFold<L> for Set<T> {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Self {
        self.iter().map(|e| e.substitute(substitution_fn)).collect()
    }
}

impl<L: Language, T: CoreFold<L>> CoreFold<L> for Option<T> {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Self {
        self.as_ref().map(|e| e.substitute(substitution_fn))
    }
}

impl<L: Language, T: CoreFold<L>> CoreFold<L> for Arc<T> {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Self {
        let data = T::substitute(self, substitution_fn);
        Arc::new(data)
    }
}

impl<L: Language> CoreFold<L> for usize {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for u8 {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for u16 {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for u32 {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for u64 {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for i8 {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for i16 {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for i32 {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for i64 {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for isize {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for () {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {}
}

impl<L: Language, A: CoreFold<L>, B: CoreFold<L>> CoreFold<L> for (A, B) {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Self {
        let (a, b) = self;
        (a.substitute(substitution_fn), b.substitute(substitution_fn))
    }
}

impl<L: Language, A: CoreFold<L>, B: CoreFold<L>, C: CoreFold<L>> CoreFold<L> for (A, B, C) {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Self {
        let (a, b, c) = self;
        (
            a.substitute(substitution_fn),
            b.substitute(substitution_fn),
            c.substitute(substitution_fn),
        )
    }
}
