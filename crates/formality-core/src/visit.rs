use std::sync::Arc;

use crate::{collections::Set, language::Language, variable::Variable};

pub trait Visit<L: Language> {
    /// Extract the list of free variables (for the purposes of this function, defined by `Variable::is_free`).
    /// The list may contain duplicates and must be in a determinstic order (though the order itself isn't important).
    fn free_variables(&self) -> Vec<Variable<L>>;

    /// Measures the overall size of the term by counting constructors etc.
    /// Used to determine overflow.
    fn size(&self) -> usize;

    /// Asserts various validity constraints and panics if they are not held.
    /// These validition constraints should never fail unless there is a bug in our logic.
    /// This is to aid with fuzzing and bug detection.
    fn assert_valid(&self);

    /// True if this term references only universal variables.
    /// This means that it contains no existential variables.
    /// If this is a goal, then when we prove it true, we don't expect any substitution.
    /// This is similar, but not *identical*, to the commonly used term "ground term",
    /// which in Prolog refers to a term that contains no variables. The difference here
    /// is that the term may contain variables, but only those instantiated universally (∀).
    fn references_only_universal_variables(&self) -> bool {
        self.free_variables().iter().all(|v| match v {
            Variable::UniversalVar(_) => true,
            Variable::ExistentialVar(_) => false,
            Variable::BoundVar(_) => false,
        })
    }
}

impl<L: Language, T: Visit<L>> Visit<L> for Vec<T> {
    fn free_variables(&self) -> Vec<Variable<L>> {
        self.iter().flat_map(|e| e.free_variables()).collect()
    }

    fn size(&self) -> usize {
        self.iter().map(|e| e.size()).sum()
    }

    fn assert_valid(&self) {
        self.iter().for_each(|e| e.assert_valid());
    }
}

impl<L: Language, T: Visit<L> + Ord> Visit<L> for Set<T> {
    fn free_variables(&self) -> Vec<Variable<L>> {
        self.iter().flat_map(|e| e.free_variables()).collect()
    }

    fn size(&self) -> usize {
        self.iter().map(|e| e.size()).sum()
    }

    fn assert_valid(&self) {
        self.iter().for_each(|e| e.assert_valid());
    }
}

impl<L: Language, T: Visit<L>> Visit<L> for Option<T> {
    fn free_variables(&self) -> Vec<Variable<L>> {
        self.iter().flat_map(|e| e.free_variables()).collect()
    }

    fn size(&self) -> usize {
        self.as_ref().map(|e| e.size()).unwrap_or(0)
    }

    fn assert_valid(&self) {
        self.iter().for_each(|e| e.assert_valid());
    }
}

impl<L: Language, T: Visit<L> + ?Sized> Visit<L> for Arc<T> {
    fn free_variables(&self) -> Vec<Variable<L>> {
        T::free_variables(self)
    }

    fn size(&self) -> usize {
        T::size(self)
    }

    fn assert_valid(&self) {
        T::assert_valid(self)
    }
}

impl<L: Language> Visit<L> for usize {
    fn free_variables(&self) -> Vec<Variable<L>> {
        vec![]
    }

    fn size(&self) -> usize {
        1
    }

    fn assert_valid(&self) {}
}

impl<L: Language> Visit<L> for u32 {
    fn free_variables(&self) -> Vec<Variable<L>> {
        vec![]
    }

    fn size(&self) -> usize {
        1
    }

    fn assert_valid(&self) {}
}

impl<L: Language> Visit<L> for u128 {
    fn free_variables(&self) -> Vec<Variable<L>> {
        vec![]
    }

    fn size(&self) -> usize {
        std::mem::size_of::<Self>()
    }

    fn assert_valid(&self) {}
}

impl<L: Language> Visit<L> for () {
    fn free_variables(&self) -> Vec<Variable<L>> {
        vec![]
    }

    fn size(&self) -> usize {
        0
    }

    fn assert_valid(&self) {}
}

impl<L: Language, A: Visit<L>, B: Visit<L>> Visit<L> for (A, B) {
    fn free_variables(&self) -> Vec<Variable<L>> {
        let (a, b) = self;
        let mut fv = vec![];
        fv.extend(a.free_variables());
        fv.extend(b.free_variables());
        fv
    }

    fn size(&self) -> usize {
        let (a, b) = self;
        a.size() + b.size()
    }

    fn assert_valid(&self) {
        let (a, b) = self;
        a.assert_valid();
        b.assert_valid();
    }
}

impl<L: Language, A: Visit<L>, B: Visit<L>, C: Visit<L>> Visit<L> for (A, B, C) {
    fn free_variables(&self) -> Vec<Variable<L>> {
        let (a, b, c) = self;
        let mut fv = vec![];
        fv.extend(a.free_variables());
        fv.extend(b.free_variables());
        fv.extend(c.free_variables());
        fv
    }

    fn size(&self) -> usize {
        let (a, b, c) = self;
        a.size() + b.size() + c.size()
    }

    fn assert_valid(&self) {
        let (a, b, c) = self;
        a.assert_valid();
        b.assert_valid();
        c.assert_valid();
    }
}

impl<L: Language, A: Visit<L> + ?Sized> Visit<L> for &A {
    fn free_variables(&self) -> Vec<Variable<L>> {
        A::free_variables(self)
    }

    fn size(&self) -> usize {
        A::size(self)
    }

    fn assert_valid(&self) {
        A::assert_valid(self)
    }
}

impl<L: Language, A: Visit<L>> Visit<L> for [A] {
    fn free_variables(&self) -> Vec<Variable<L>> {
        self.iter().flat_map(|e| A::free_variables(e)).collect()
    }

    fn size(&self) -> usize {
        self.iter().map(|e| A::size(e)).sum()
    }

    fn assert_valid(&self) {
        self.iter().for_each(|e| A::assert_valid(e));
    }
}
