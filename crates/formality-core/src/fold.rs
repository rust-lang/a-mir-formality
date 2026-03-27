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
    /// The type produced by substitution. For most types this is `Self` (substituting a `Ty`
    /// yields a `Ty`). For reference types like `(&A, &B)` the output is the owned tuple `(A, B)`.
    type Output;

    /// Replace uses of variables with values from the substitution.
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Self::Output;

    /// Produce a version of this term where any debruijn indices which appear free are incremented by one.
    fn shift_in(&self) -> Self::Output {
        self.substitute(&mut |v| Some(v.shift_in().upcast()))
    }

    /// Replace all appearances of free variable `v` with `p`.
    fn replace_free_var(
        &self,
        v: impl Upcast<CoreVariable<L>>,
        p: impl Upcast<CoreParameter<L>>,
    ) -> Self::Output {
        let v: CoreVariable<L> = v.upcast();
        let p: CoreParameter<L> = p.upcast();
        assert!(v.is_free());
        assert!(v.kind() == p.kind());
        self.substitute(&mut |v1| if v == v1 { Some(p.clone()) } else { None })
    }
}

impl<L: Language, T: CoreFold<L>> CoreFold<L> for Vec<T> {
    type Output = Vec<T::Output>;

    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Vec<T::Output> {
        self.iter().map(|e| e.substitute(substitution_fn)).collect()
    }
}

impl<L: Language, T: CoreFold<L> + Ord> CoreFold<L> for Set<T>
where
    T::Output: Ord,
{
    type Output = Set<T::Output>;

    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Set<T::Output> {
        self.iter().map(|e| e.substitute(substitution_fn)).collect()
    }
}

impl<L: Language, T: CoreFold<L>> CoreFold<L> for Option<T> {
    type Output = Option<T::Output>;

    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Option<T::Output> {
        self.as_ref().map(|e| e.substitute(substitution_fn))
    }
}

impl<L: Language, T: CoreFold<L>> CoreFold<L> for Arc<T> {
    type Output = Arc<T::Output>;

    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> Arc<T::Output> {
        let data = T::substitute(self, substitution_fn);
        Arc::new(data)
    }
}

impl<L: Language, T: CoreFold<L>> CoreFold<L> for &T {
    type Output = T::Output;

    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> T::Output {
        CoreFold::substitute(*self, substitution_fn)
    }
}

impl<L: Language> CoreFold<L> for bool {
    type Output = Self;

    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for usize {
    type Output = Self;

    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for u8 {
    type Output = Self;

    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for u16 {
    type Output = Self;

    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for u32 {
    type Output = Self;

    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for u64 {
    type Output = Self;

    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for i8 {
    type Output = Self;

    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for i16 {
    type Output = Self;

    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for i32 {
    type Output = Self;

    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for i64 {
    type Output = Self;

    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for isize {
    type Output = Self;

    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {
        *self
    }
}

impl<L: Language> CoreFold<L> for () {
    type Output = Self;

    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, L>) -> Self {}
}

impl<L: Language, A: CoreFold<L>, B: CoreFold<L>> CoreFold<L> for (A, B) {
    type Output = (A::Output, B::Output);

    fn substitute(&self, substitution_fn: SubstitutionFn<'_, L>) -> (A::Output, B::Output) {
        let (a, b) = self;
        (a.substitute(substitution_fn), b.substitute(substitution_fn))
    }
}

impl<L: Language, A: CoreFold<L>, B: CoreFold<L>, C: CoreFold<L>> CoreFold<L> for (A, B, C) {
    type Output = (A::Output, B::Output, C::Output);

    fn substitute(
        &self,
        substitution_fn: SubstitutionFn<'_, L>,
    ) -> (A::Output, B::Output, C::Output) {
        let (a, b, c) = self;
        (
            a.substitute(substitution_fn),
            b.substitute(substitution_fn),
            c.substitute(substitution_fn),
        )
    }
}
