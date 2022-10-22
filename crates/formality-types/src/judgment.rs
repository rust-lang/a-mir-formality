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
macro_rules! judgment {
    (($judgment:ty => $output:ty) $(($($rule:tt)*))*) => {
        impl $crate::judgment::Judgment for $judgment {
            type Output = $output;

            fn stack() -> &'static LocalKey<RefCell<JudgmentStack<$judgment>>> {
                thread_local! {
                    static R: RefCell<JudgmentStack<$judgment>> = Default::default()
                }
                &R
            }

            fn build_rules(builder: &mut $crate::judgment::JudgmentBuilder<Self>) {
                $crate::push_rules!(
                    builder,
                    $(($($rule)*))*
                );
            }
        }
    };
}

/// push_rules! allows construction of inference rules using a more logic-like notation.
///
/// The macro input looks like: `push_rules!(builder, (...) (...) (...))` where each
/// parenthesized group `(...)` is an inference rule. Inference rules are written like so:
///
/// ```ignore
/// (
///     ( /* condition 1 */) // each condition is a parenthesized group
///     ( /* condition 2 */)
///     -------------------- // 3 or more `-` separate the condition from the conclusion
///     ( /* conclusion */)  // as is the conclusion
/// )
/// ```
///
/// The conditions can be the following
///
/// * `(<expr> => <binding>)` -- used to apply judgments, but really `<expr>` can be anything with an `into_iter` method.
/// * `(if <expr>)`
/// * `(let <binding> = <expr>)`
///
/// The conclusions can be the following
///
/// * `(<pat> => <binding>)
#[macro_export]
macro_rules! push_rules {
    ($builder:expr, $($rule:tt)*) => {
        $($crate::push_rules!(@rule ($builder) $rule);)*
    };

    // `@rule (builder) rule` phase: invoked for each rule, emits `push_rule` call

    (@rule ($builder:expr) ($($m:tt)*)) => {
        $builder.push_rule($crate::push_rules!(@accum () $($m)*))
    };

    // `@accum (conditions)` phase: accumulates the contents of a given rule,
    // pushing tokens into `conditions` until the `-----` and conclusion are found.

    (@accum ($($m:tt)*) ---$(-)* ($p:pat => $v:expr)) => {
        // Found the conclusion.
        |$p| -> Vec<_> {
            $crate::push_rules!(@body ($v) $($m)*).into_iter().collect()
        }
    };

    (@accum ($($m:tt)*) ($($n:tt)*) $($o:tt)*) => {
        // Push the condition into the list `$m`.
        $crate::push_rules!(@accum ($($m)* ($($n)*)) $($o)*)
    };

    // `@body (v)` phase: processes the conditions, generating the code
    // to evaluate the rule. This is effectively an iterator chain. The
    // expression `v` is carried in from the conclusion and forms the final
    // output of this rule, once all the conditions are evaluated.

    (@body ($v:expr) (if $c:expr) $($m:tt)*) => {
        if $c {
            $crate::push_rules!(@body ($v) $($m)*)
        } else {
            None
        }
    };

    (@body ($v:expr) ($i:expr => $p:pat) $($m:tt)*) => {
        $i.into_iter().flat_map(move |$p| {
            $crate::push_rules!(@body ($v) $($m)*)
        })
    };

    (@body ($v:expr) (let $p:pat = $i:expr) $($m:tt)*) => {
        let $p = $i;
        $crate::push_rules!(@body ($v) $($m)*)
    };

    (@body ($v:expr)) => {
        Some($v)
    };
}
