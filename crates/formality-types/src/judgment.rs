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
macro_rules! judgment_fn {
    (
        $v:vis fn $name:ident($($input_name:ident : $input_ty:ty),* $(,)?) => $output:ty {
            $(($($rule:tt)*))*
        }
    ) => {
        $v fn $name($($input_name : impl $crate::cast::Upcast<$input_ty>),*) -> $crate::collections::Set<$output> {
            #[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Clone)]
            struct __JudgmentStruct($($input_ty),*);

            $crate::cast_impl!(__JudgmentStruct);

            impl std::iter::IntoIterator for __JudgmentStruct {
                type Item = $output;

                type IntoIter = std::collections::btree_set::IntoIter<Self::Item>;

                fn into_iter(self) -> Self::IntoIter {
                    $crate::judgment::Judgment::apply(&self).into_iter()
                }
            }

            impl $crate::judgment::Judgment for __JudgmentStruct {
                type Output = $output;

                fn stack() -> &'static std::thread::LocalKey<$crate::judgment::JudgmentStack<__JudgmentStruct, $output>> {
                    thread_local! {
                        static R: $crate::judgment::JudgmentStack<__JudgmentStruct, $output> = Default::default()
                    }
                    &R
                }

                fn build_rules(builder: &mut $crate::judgment::JudgmentBuilder<Self>) {
                    $crate::push_rules!(
                        $name,
                        builder,
                        $output,
                        $(($($rule)*))*
                    );
                }
            }

            $(let $input_name: $input_ty = $crate::cast::Upcast::upcast($input_name);)*
            $crate::judgment::Judgment::apply(&__JudgmentStruct($($input_name),*))
        }
    }
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
    ($judgment_name:ident, $builder:expr, $output_ty:ty, $($rule:tt)*) => {
        $($crate::push_rules!(@rule ($judgment_name, $builder, $output_ty) $rule);)*
    };

    // `@rule (builder) rule` phase: invoked for each rule, emits `push_rule` call

    (@rule ($judgment_name:ident, $builder:expr, $output_ty:ty) ($($m:tt)*)) => {
        $builder.push_rule($crate::push_rules!(@accum ($judgment_name, $output_ty, ) $($m)*))
    };

    // `@accum (conditions)` phase: accumulates the contents of a given rule,
    // pushing tokens into `conditions` until the `-----` and conclusion are found.

    (@accum ($judgment_name:ident, $output_ty:ty, $($m:tt)*)
        ---$(-)* ($n:literal)
        ($conclusion_name:ident($($p:pat),* $(,)?) => $v:expr)
    ) => {
        // Found the conclusion.
        |v| -> Vec<$output_ty> {
            let mut output = vec![];

            // give the user a type error if the name they gave
            // in the conclusion is not the same as the name of the
            // function
            struct WrongJudgmentNameInConclusion;
            const _: WrongJudgmentNameInConclusion = {
                let $judgment_name = WrongJudgmentNameInConclusion;
                $conclusion_name
            };

            #[allow(irrefutable_let_patterns)]
            if let __JudgmentStruct($($p),*) = v {
                tracing::debug_span!("matched rule", rule = $n).in_scope(|| {
                    $crate::push_rules!(@body ($v, output) $($m)*);
                });
            }
            output
        }
    };

    (@accum ($judgment_name:ident, $output_ty:ty, $($m:tt)*) ($($n:tt)*) $($o:tt)*) => {
        // Push the condition into the list `$m`.
        $crate::push_rules!(@accum ($judgment_name, $output_ty, $($m)* ($($n)*)) $($o)*)
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
