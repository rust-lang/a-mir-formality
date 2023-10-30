/// A term representing a variable.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Variable {
    /// A "universal free variable" is a variable that appears
    /// free in all terms because it is bound in the environment.
    /// Universal means that it arose from a "forall" binder.
    /// Universal variables are a kind of placeholder meant to represent
    /// "some value" about which you know nothing except what you are
    /// told to assume.
    UniversalVar(UniversalVar),

    /// An "existential free variable" is a variable that appears
    /// free in all terms because it is bound in the environment.
    /// Existential means that it arose from a "exists" binder.
    /// Existential variables are a kind of placeholder for which
    /// you will eventually find some specific value, so the rules typically
    /// accumulate constraints.
    ExistentialVar(ExistentialVar),

    /// A bound variable is one that is bound by some enclosing `Binder`
    /// in this term (or a binder about to be constructex; see `fresh_bound_var`).
    BoundVar(BoundVar),
}

cast_impl!(Variable);

impl Variable {
    pub fn kind(&self) -> ParameterKind {
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

impl Visit for Variable {
    fn free_variables(&self) -> Vec<Variable> {
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
