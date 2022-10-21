use std::{cell::RefCell, collections::BTreeSet, sync::Arc, thread::LocalKey};

use crate::derive_links::Term;

use self::stack::JudgmentStack;

mod apply;
mod builder;
mod stack;
mod test;

pub use self::builder::JudgmentBuilder;

pub trait Judgment: Term {
    type Output: Term;

    fn stack() -> &'static LocalKey<RefCell<JudgmentStack<Self>>>;

    fn build_rules(builder: &mut JudgmentBuilder<Self>);

    fn apply(&self) -> BTreeSet<Self::Output> {
        apply::JudgmentApply(self).apply()
    }

    fn into_iter(self) -> std::collections::btree_set::IntoIter<Self::Output> {
        self.apply().into_iter()
    }
}

#[derive(Clone)]
struct InferenceRule<I, O> {
    closure: Arc<dyn Fn(&I) -> Vec<O> + Send>,
}
