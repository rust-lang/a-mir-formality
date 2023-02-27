use std::{fmt::Debug, hash::Hash, sync::Arc};

use crate::{
    cast::{DowncastFrom, Upcast},
    collections::Set,
    derive_links::Variable,
    fold::Fold,
    grammar::{Binder, Lt, Ty, Universe},
    parse::Parse,
};

pub trait Term:
    Clone + Fold + Parse + Ord + Eq + Hash + Debug + Upcast<Self> + DowncastFrom<Self> + 'static + Sized
{
    /// True if this term references only placeholder variables.
    /// This means that it contains no inference variables.
    /// If this is a goal, then when we prove it true, we don't expect any substitution.
    /// This is similar, but not *identical*, to the commonly used term "ground term",
    /// which in Prolog refers to a term that contains no variables. The difference here
    /// is that the term may contain variables, but only those instantiated universally (∀).
    fn references_only_placeholder_variables(&self) -> bool {
        self.free_variables().iter().all(|v| match v {
            Variable::PlaceholderVar(_) => true,
            Variable::InferenceVar(_) => false,
            Variable::BoundVar(_) => false,
        })
    }

    /// Maximum universe of of any placeholders appearing in this term
    fn max_universe(&self) -> Universe {
        self.free_variables()
            .iter()
            .map(|v| match v {
                Variable::PlaceholderVar(p) => p.universe,
                Variable::InferenceVar(_) => Universe::ROOT,
                Variable::BoundVar(_) => Universe::ROOT,
            })
            .max()
            .unwrap_or(Universe::ROOT)
    }
}

impl<T: Term> Term for Vec<T> {}

impl<T: Term> Term for Set<T> {}

impl<T: Term> Term for Option<T> {}

impl<T: Term> Term for Arc<T> {}

impl Term for Ty {}

impl Term for Lt {}

impl Term for usize {}

impl Term for u32 {}

impl<A: Term, B: Term> Term for (A, B) {}

impl<T: Term> Term for Binder<T> {}
