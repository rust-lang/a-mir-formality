use std::{
    cell::RefCell, collections::BTreeSet, fmt::Debug, hash::Hash, sync::Arc, thread::LocalKey,
};

use crate::fixed_point::FixedPointStack;

mod apply;
mod builder;
mod test_filtered;
mod test_reachable;

pub use self::builder::JudgmentBuilder;

pub type JudgmentStack<J, O> = RefCell<FixedPointStack<J, BTreeSet<O>>>;

pub trait Judgment:
    Debug + Ord + Hash + Clone + Sized + 'static + IntoIterator<Item = Self::Output>
{
    type Output: Debug + Ord + Hash + Clone;

    fn stack() -> &'static LocalKey<JudgmentStack<Self, Self::Output>>;

    fn build_rules(builder: &mut JudgmentBuilder<Self>);

    fn apply(&self) -> BTreeSet<Self::Output> {
        apply::JudgmentApply(self).apply()
    }
}

#[derive(Clone)]
struct InferenceRule<I, O> {
    closure: Arc<dyn Fn(&I) -> Vec<O> + Send>,
}

#[macro_export]
macro_rules! judgment {
    (($judgment:ty => $output:ty) $(($($rule:tt)*))*) => {
        impl std::iter::IntoIterator for $judgment {
            type Item = $output;

            type IntoIter = std::collections::btree_set::IntoIter<Self::Item>;

            fn into_iter(self) ->  Self::IntoIter {
                self.apply().into_iter()
            }
        }

        impl $crate::judgment::Judgment for $judgment {
            type Output = $output;

            fn stack() -> &'static std::thread::LocalKey<$crate::judgment::JudgmentStack<$judgment, $output>> {
                thread_local! {
                    static R: $crate::judgment::JudgmentStack<$judgment, $output> = Default::default()
                }
                &R
            }

            fn build_rules(builder: &mut $crate::judgment::JudgmentBuilder<Self>) {
                $crate::push_rules!(
                    builder,
                    $output,
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
/// * `(if let <pat> = <expr>)`
/// * `(let <binding> = <expr>)`
///
/// The conclusions can be the following
///
/// * `(<pat> => <binding>)
#[macro_export]
macro_rules! push_rules {
    ($builder:expr, $output_ty:ty, $($rule:tt)*) => {
        $($crate::push_rules!(@rule ($builder, $output_ty) $rule);)*
    };

    // `@rule (builder) rule` phase: invoked for each rule, emits `push_rule` call

    (@rule ($builder:expr, $output_ty:ty) ($($m:tt)*)) => {
        $builder.push_rule($crate::push_rules!(@accum ($output_ty, ) $($m)*))
    };

    // `@accum (conditions)` phase: accumulates the contents of a given rule,
    // pushing tokens into `conditions` until the `-----` and conclusion are found.

    (@accum ($output_ty:ty, $($m:tt)*) ---$(-)* ($n:literal) ($p:pat => $v:expr)) => {
        // Found the conclusion.
        |v| -> Vec<$output_ty> {
            let mut output = vec![];
            #[allow(irrefutable_let_patterns)]
            if let $p = v {
                tracing::debug_span!("matched rule", rule = $n).in_scope(|| {
                    $crate::push_rules!(@body ($v, output) $($m)*);
                });
            }
            output
        }
    };

    (@accum ($output_ty:ty, $($m:tt)*) ($($n:tt)*) $($o:tt)*) => {
        // Push the condition into the list `$m`.
        $crate::push_rules!(@accum ($output_ty, $($m)* ($($n)*)) $($o)*)
    };

    // `@body (v)` phase: processes the conditions, generating the code
    // to evaluate the rule. This is effectively an iterator chain. The
    // expression `v` is carried in from the conclusion and forms the final
    // output of this rule, once all the conditions are evaluated.

    (@body ($v:expr, $output:ident) (if $c:expr) $($m:tt)*) => {
        if $c {
            $crate::push_rules!(@body ($v, $output) $($m)*);
        }
    };

    (@body ($v:expr, $output:ident) (assert $c:expr) $($m:tt)*) => {
        assert!($c);
        $crate::push_rules!(@body ($v, $output) $($m)*);
    };

    (@body ($v:expr, $output:ident) (if let $p:pat = $e:expr) $($m:tt)*) => {
        if let $p = $e {
            $crate::push_rules!(@body ($v, $output) $($m)*);
        }
    };

    (@body ($v:expr, $output:ident) ($i:expr => $p:pat) $($m:tt)*) => {
        for $p in $i {
            $crate::push_rules!(@body ($v, $output) $($m)*);
        }
    };

    (@body ($v:expr, $output:ident) (let $p:pat = $i:expr) $($m:tt)*) => {
        {
            let $p = $i;
            $crate::push_rules!(@body ($v, $output) $($m)*);
        }
    };

    (@body ($v:expr, $output:ident)) => {
        $output.push($v)
    };
}
