use std::{cell::RefCell, collections::BTreeSet, sync::Arc, thread::LocalKey};

use crate::derive_links::Term;

use self::stack::JudgmentStack;

mod apply;
mod builder;
mod stack;
mod test_filtered;
mod test_reachable;

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

#[macro_export]
macro_rules! push_rules {
    ($builder:expr, $($rule:tt)*) => {
        $($crate::push_rules!(@rule ($builder) $rule);)*
    };

    (@rule ($builder:expr) (($p:pat => $v:expr) :- $($m:tt)*)) => {
        $builder.push_rule(|$p| -> Vec<_> {
            $crate::push_rules!(@body ($v) $($m)*).into_iter().collect()
        })
    };

    (@body ($v:expr) if $c:expr, $($m:tt)*) => {
        if $c {
            $crate::push_rules!(@body ($v) $($m)*)
        } else {
            None
        }
    };

    (@body ($v:expr) $i:expr => $p:pat, $($m:tt)*) => {
        $i.into_iter().flat_map(move |$p| {
            $crate::push_rules!(@body ($v) $($m)*)
        })
    };

    (@body ($v:expr) let $p:pat = $i:expr, $($m:tt)*) => {
        let $p = $i;
        $crate::push_rules!(@body ($v) $($m)*)
    };


    (@body ($v:expr)) => {
        Some($v)
    };
}
