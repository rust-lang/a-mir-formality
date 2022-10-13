use crate::grammar::{BoundVar, Parameter, Substitution, VarIndex, Variable};

pub trait Fold {
    /// Replace uses of variables with values from the substitution.
    fn substitute(&self, substitution_fn: &mut impl FnMut(Variable) -> Parameter) -> Self;

    /// Extract the list of free variables (variables not captured by a binder).
    fn free_variables(&self, substitution: &Substitution) -> Vec<Variable>;

    /// Produce a version of this term with all debruijn indices incremented by one.
    fn shift_in(&self, binders: usize) -> Self;
}

pub trait Folder {
    fn fold_bound_var(&self, index: VarIndex) -> Parameter {
        BoundVar {
            debruijn: None,
            index,
        }
        .into()
    }
}
