use crate::cast::Upcast;
use crate::language::Kind;
use crate::{derive_links::Visit, language::Language};

/// A term representing a variable.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Variable<L: Language> {
    /// A "universal free variable" is a variable that appears
    /// free in all terms because it is bound in the environment.
    /// Universal means that it arose from a "forall" binder.
    /// Universal variables are a kind of placeholder meant to represent
    /// "some value" about which you know nothing except what you are
    /// told to assume.
    UniversalVar(UniversalVar<L>),

    /// An "existential free variable" is a variable that appears
    /// free in all terms because it is bound in the environment.
    /// Existential means that it arose from a "exists" binder.
    /// Existential variables are a kind of placeholder for which
    /// you will eventually find some specific value, so the rules typically
    /// accumulate constraints.
    ExistentialVar(ExistentialVar<L>),

    /// A bound variable is one that is bound by some enclosing `Binder`
    /// in this term (or a binder about to be constructex; see `fresh_bound_var`).
    BoundVar(BoundVar<L>),
}

impl<L: Language> Variable<L> {
    pub fn kind(&self) -> Kind<L> {
        match self {
            Variable::UniversalVar(v) => v.kind,
            Variable::ExistentialVar(v) => v.kind,
            Variable::BoundVar(v) => v.kind,
        }
    }

    /// Shift a variable in through `binders` binding levels.
    /// Only affects bound variables.
    pub fn shift_in(&self) -> Self {
        if let Variable::BoundVar(BoundVar {
            debruijn: Some(db),
            var_index,
            kind,
        }) = self
        {
            BoundVar {
                debruijn: Some(db.shift_in()),
                var_index: *var_index,
                kind: *kind,
            }
            .upcast()
        } else {
            *self
        }
    }

    /// Shift a variable out through `binders` binding levels.
    /// Only affects bound variables. Returns None if the variable
    /// is bound within those binding levels.
    pub fn shift_out(&self) -> Option<Self> {
        if let Variable::BoundVar(BoundVar {
            debruijn: Some(db),
            var_index,
            kind,
        }) = self
        {
            db.shift_out().map(|db1| {
                BoundVar {
                    debruijn: Some(db1),
                    var_index: *var_index,
                    kind: *kind,
                }
                .upcast()
            })
        } else {
            Some(*self)
        }
    }

    /// A variable is *free* (i.e., not bound by any internal binder)
    /// if it is an existential variable, a universal, or has a debruijn
    /// index of `None`. The latter occurs when you `open` a binder (and before
    /// you close it back up again).
    pub fn is_free(&self) -> bool {
        match self {
            Variable::UniversalVar(_)
            | Variable::ExistentialVar(_)
            | Variable::BoundVar(BoundVar {
                debruijn: None,
                var_index: _,
                kind: _,
            }) => true,

            Variable::BoundVar(BoundVar {
                debruijn: Some(_),
                var_index: _,
                kind: _,
            }) => false,
        }
    }

    pub fn is_universal(&self) -> bool {
        match self {
            Variable::UniversalVar(_) => true,
            Variable::ExistentialVar(_) => false,
            Variable::BoundVar(_) => false,
        }
    }
}

impl<L: Language> Visit<L> for Variable<L> {
    fn free_variables(&self) -> Vec<Variable<L>> {
        if self.is_free() {
            vec![*self]
        } else {
            vec![]
        }
    }

    fn size(&self) -> usize {
        1
    }

    fn assert_valid(&self) {}
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExistentialVar<L: Language> {
    pub kind: Kind<L>,
    pub var_index: VarIndex,
}

impl<L: Language> Visit<L> for ExistentialVar<L> {
    fn free_variables(&self) -> Vec<Variable<L>> {
        vec![self.upcast()]
    }

    fn size(&self) -> usize {
        1
    }

    fn assert_valid(&self) {}
}

/// A *universal variable* is a dummy variable about which nothing is known except
/// that which we see in the environment. When we want to prove something
/// is true for all `T` (`∀T`), we replace `T` with a universal variable.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UniversalVar<L: Language> {
    pub kind: Kind<L>,
    pub var_index: VarIndex,
}

/// Identifies a bound variable.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoundVar<L: Language> {
    /// Identifies the binder that contained this variable, counting "outwards".
    /// When you create a binder with `Binder::new`,
    /// When you open a Binder, you get back `Bound
    pub debruijn: Option<DebruijnIndex>,
    pub var_index: VarIndex,
    pub kind: Kind<L>,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DebruijnIndex {
    pub index: usize,
}

impl DebruijnIndex {
    pub const INNERMOST: DebruijnIndex = DebruijnIndex { index: 0 };

    /// Adjust this debruijn index through a binder level.
    pub fn shift_in(&self) -> Self {
        DebruijnIndex {
            index: self.index + 1,
        }
    }

    /// Adjust this debruijn index *outward* through a binder level, if possible.
    pub fn shift_out(&self) -> Option<Self> {
        if self.index > 0 {
            Some(DebruijnIndex {
                index: self.index - 1,
            })
        } else {
            None
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarIndex {
    pub index: usize,
}

impl VarIndex {
    pub const ZERO: VarIndex = VarIndex { index: 0 };
}

impl std::ops::Add<usize> for VarIndex {
    type Output = VarIndex;

    fn add(self, rhs: usize) -> Self::Output {
        VarIndex {
            index: self.index + rhs,
        }
    }
}

mod cast_impls;
mod debug_impls;
