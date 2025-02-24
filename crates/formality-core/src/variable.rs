use crate::cast::Upcast;
use crate::fuzz::Fuzzable;
use crate::language::CoreKind;
use crate::language::Language;
use crate::visit::CoreVisit;

/// A term representing a variable.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CoreVariable<L: Language> {
    /// A "universal free variable" is a variable that appears
    /// free in all terms because it is bound in the environment.
    /// Universal means that it arose from a "forall" binder.
    /// Universal variables are a kind of placeholder meant to represent
    /// "some value" about which you know nothing except what you are
    /// told to assume.
    UniversalVar(CoreUniversalVar<L>),

    /// An "existential free variable" is a variable that appears
    /// free in all terms because it is bound in the environment.
    /// Existential means that it arose from a "exists" binder.
    /// Existential variables are a kind of placeholder for which
    /// you will eventually find some specific value, so the rules typically
    /// accumulate constraints.
    ExistentialVar(CoreExistentialVar<L>),

    /// A bound variable is one that is bound by some enclosing `Binder`
    /// in this term (or a binder about to be constructex; see `fresh_bound_var`).
    BoundVar(CoreBoundVar<L>),
}

impl<L: Language> CoreVariable<L> {
    pub fn kind(&self) -> CoreKind<L> {
        match self {
            CoreVariable::UniversalVar(v) => v.kind,
            CoreVariable::ExistentialVar(v) => v.kind,
            CoreVariable::BoundVar(v) => v.kind,
        }
    }

    /// Shift a variable in through `binders` binding levels.
    /// Only affects bound variables.
    pub fn shift_in(&self) -> Self {
        if let CoreVariable::BoundVar(CoreBoundVar {
            debruijn: Some(db),
            var_index,
            kind,
        }) = self
        {
            CoreBoundVar {
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
        if let CoreVariable::BoundVar(CoreBoundVar {
            debruijn: Some(db),
            var_index,
            kind,
        }) = self
        {
            db.shift_out().map(|db1| {
                CoreBoundVar {
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
            CoreVariable::UniversalVar(_)
            | CoreVariable::ExistentialVar(_)
            | CoreVariable::BoundVar(CoreBoundVar {
                debruijn: None,
                var_index: _,
                kind: _,
            }) => true,

            CoreVariable::BoundVar(CoreBoundVar {
                debruijn: Some(_),
                var_index: _,
                kind: _,
            }) => false,
        }
    }

    pub fn is_universal(&self) -> bool {
        match self {
            CoreVariable::UniversalVar(_) => true,
            CoreVariable::ExistentialVar(_) => false,
            CoreVariable::BoundVar(_) => false,
        }
    }
}

impl<L: Language> CoreVisit<L> for CoreVariable<L> {
    fn free_variables(&self) -> Vec<CoreVariable<L>> {
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
pub struct CoreExistentialVar<L: Language> {
    pub kind: CoreKind<L>,
    pub var_index: VarIndex,
}

impl<L: Language> CoreVisit<L> for CoreExistentialVar<L> {
    fn free_variables(&self) -> Vec<CoreVariable<L>> {
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
pub struct CoreUniversalVar<L: Language> {
    pub kind: CoreKind<L>,
    pub var_index: VarIndex,
}

/// Identifies a bound variable.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CoreBoundVar<L: Language> {
    /// Identifies the binder that contained this variable, counting "outwards".
    /// When you create a binder with `Binder::new`,
    /// When you open a Binder, you get back `Bound
    pub debruijn: Option<DebruijnIndex>,
    pub var_index: VarIndex,
    pub kind: CoreKind<L>,
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

impl<L: Language> Fuzzable<L> for CoreVariable<L> {
    fn estimate_cardinality(cx: &mut crate::fuzz::FuzzCx<'_, L>) -> f64 {
        cx.enter_estimate_cardinality::<Self>(|guard| guard.estimated_free_variable_cardinality())
    }

    fn fuzz(cx: &mut crate::fuzz::FuzzCx<'_, L>) -> Option<Self> {
        cx.enter_fuzz::<Self>(|guard| guard.pick_free_variable())
    }
}

mod cast_impls;
mod debug_impls;
