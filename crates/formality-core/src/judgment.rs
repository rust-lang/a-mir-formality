use std::cell::RefCell;

use crate::{fixed_point::FixedPointStack, Fallible, Map};

mod assertion;
pub use assertion::JudgmentAssertion;

mod proven_set;
pub use proven_set::{
    insert_smallest_proof, member_of, CheckProven, EachProof, FailedJudgment, FailedRule,
    ProofTree, Proven, ProvenSet, RuleFailureCause,
};

mod test_fallible;
mod test_filtered;
mod test_for_all;
mod test_reachable;

pub type JudgmentStack<J, O> = RefCell<FixedPointStack<J, Map<O, ProofTree>>>;

/// `judgment_fn!` allows construction of inference rules using a more logic-like notation.
///
/// The macro input looks like so:
///
/// ```ignore
/// (
///     ( /* condition 1 */)  // each condition is a parenthesized group
///     ( /* condition 2 */)
///     ( /* condition 3 */)! // `!` is optional and indicates match commit point, see below
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
///
/// ## Failure reporting and match commit points
///
/// When we fail to prove a judgment, we'll produce a failure that includes
/// all rules that partially matched. By default this lits includes every
/// rule that matches the patterns in its conclusion.
/// Sometimes this is annoyingly verbose.
/// You can place a `!` after a condition to mark it as a "match commit point".
/// Rules that fail before reaching the match commit point will not be included
/// in the failure result.
#[macro_export]
macro_rules! judgment_fn {
    (
        $(#[$attr:meta])*
        $v:vis fn $name:ident($($input_name:ident : $input_ty:ty),* $(,)?) => $output:ty {
            debug($($debug_input_name:ident),*)
            $(assert($assert_expr:expr))*
            $(trivial($trivial_expr:expr => $trivial_result:expr))*
            $(($($rule:tt)*))*
        }
    ) => {
        $(#[$attr])*
        $v fn $name($($input_name : impl $crate::Upcast<$input_ty>),*) -> $crate::ProvenSet<$output> {
            #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Clone)]
            struct __JudgmentStruct($($input_ty),*);

            $crate::cast_impl!(__JudgmentStruct);

            impl std::fmt::Debug for __JudgmentStruct {
                fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    let mut f = fmt.debug_struct(stringify!($name));
                    let __JudgmentStruct($($input_name),*) = self;
                    $(
                        f.field(stringify!($debug_input_name), $debug_input_name);
                    )*
                    f.finish()
                }
            }

            $(let $input_name: $input_ty = $crate::Upcast::upcast($input_name);)*

            $(
                // Assertions are preconditions.
                //
                // NB: we can't use `$crate` in this expression because of the `respan!` call,
                // which messes up `$crate` resolution. But we need the respan call for track_caller to properly
                // assign the span of the panic to the assertion expression and not the invocation of the judgment_fn
                // macro. Annoying! But our proc macros already reference `formality_core` so that seems ok.
                $crate::respan!(
                    $assert_expr
                    (
                        formality_core::judgment::JudgmentAssertion::assert($assert_expr, stringify!($assert_expr));
                    )
                );
            )*

            $(
                // Trivial cases are an (important) optimization that lets
                // you cut out all the normal rules.
                if $trivial_expr {
                    let trivial_result = $trivial_result;
                    let (file, line, column) = $crate::respan!(
                        $trivial_expr
                        ((file!(), line!(), column!()))
                    );
                    let proof_tree = $crate::judgment::ProofTree::with_all(
                        format!("trivial, as {} is true: {trivial_result:?}", stringify!($trivial_expr)),
                        Default::default(),
                        None,
                        file,
                        line,
                        column,
                        Default::default(),
                    );
                    return $crate::ProvenSet::singleton((trivial_result, proof_tree));
                }
            )*

            let mut failed_rules = $crate::set![];
            let input = __JudgmentStruct($($input_name),*);
            let output = $crate::fixed_point::fixed_point::<
                __JudgmentStruct,
                $crate::Map<$output, $crate::judgment::ProofTree>,
            >(
                // Tracing span:
                |input| {
                    let __JudgmentStruct($($input_name),*) = input;
                    tracing::debug_span!(
                        stringify!($name),
                        $(?$debug_input_name),*
                    )
                },

                // Stack:
                {
                    thread_local! {
                        static R: $crate::judgment::JudgmentStack<__JudgmentStruct, $output> = Default::default()
                    }
                    &R
                },

                // Input:
                input.clone(),

                // Default value:
                |_| Default::default(),

                // Next value:
                |input: __JudgmentStruct| {
                    let mut output: $crate::Map<$output, $crate::judgment::ProofTree> = $crate::Map::new();

                    failed_rules.clear();

                    #[allow(unused)]
                    let input_string = format!("{:?}", input);

                    $crate::push_rules!(
                        $name,
                        &input,
                        output,
                        failed_rules,
                        &input_string,
                        ($($input_name),*) => $output,
                        $(($($rule)*))*
                    );

                    output
                },
            );

            if !output.is_empty() {
                $crate::ProvenSet::proven(output)
            } else {
                $crate::ProvenSet::failed_rules(&input, failed_rules)
            }
        }
    }
}

#[macro_export]
macro_rules! push_rules {
    ($judgment_name:ident, $input_value:expr, $output:expr, $failed_rules:expr, $input_string:expr, $input_names:tt => $output_ty:ty, $($rule:tt)*) => {
        $($crate::push_rules!(@rule ($judgment_name, $input_value, $output, $failed_rules, $input_string, $input_names => $output_ty) $rule);)*
    };

    // `@rule (builder) rule` phase: invoked for each rule, emits `push_rule` call

    (@rule ($judgment_name:ident, $input_value:expr, $output:expr, $failed_rules:expr, $input_string:expr, $input_names:tt => $output_ty:ty) ($($m:tt)*)) => {
        // Start accumulating.
        $crate::push_rules!(@accum
            args($judgment_name, $input_value, $output, $failed_rules, $input_string, $input_names => $output_ty)
            accum(true;)
            input($($m)*)
        );
    };

    // `@accum ($match_index; $current_index; conditions)` phase: accumulates the contents of a given rule,
    // pushing tokens into `conditions` until the `-----` and conclusion are found.
    //
    // The `$match_index` stores the location where `!` was found. It is expected to start
    // at 0. The `current_index` is also expected to start as the expression `0`.

    (@accum
        args($judgment_name:ident, $input_value:expr, $output:expr, $failed_rules:expr, $input_string:expr, ($($input_names:ident),*) => $output_ty:ty)
        accum($match_default:tt; $($m:tt)*)
        input(
            ---$(-)* ($n:literal)
            ($conclusion_name:ident($($patterns:tt)*) => $v:expr)
        )
    ) => {
        // Found the conclusion.
        {
            $crate::respan!(
                $conclusion_name
                (
                    // give the user a type error if the name they gave
                    // in the conclusion is not the same as the name of the
                    // function
                    #[allow(dead_code)]
                    struct WrongJudgmentNameInConclusion;
                    const _: WrongJudgmentNameInConclusion = {
                        let $judgment_name = WrongJudgmentNameInConclusion;
                        $conclusion_name
                    };
                )
            );

            if let Some(__JudgmentStruct($($input_names),*)) = Some($input_value) {
                let __judgment_name = stringify!($judgment_name);
                let __attributes: Vec<(String, String)> = vec![
                    $((stringify!($input_names).to_string(), format!("{:?}", $input_names))),*
                ];
                let mut __matched = $match_default;
                $crate::push_rules!(@match
                    $conclusion_name
                    inputs($($input_names)*)
                    patterns($($patterns)*,)
                    args(@body
                        ($judgment_name; $n; $v; $output);
                        ($failed_rules, __matched, (__judgment_name, __attributes), $n);
                        $($m)*
                    )
                );
            }
        }
    };

    (@accum
        args $args:tt
        accum(true; $($m:tt)*)
        input(! $($o:tt)*)
    ) => {
        // If we see a `!` in the list, that represents a "match commit point".
        // Switch the "match default" from true to false.
        $crate::push_rules!(@accum
            args $args
            accum(false; $($m)* !)
            input($($o)*)
        )
    };

    (@accum
        args $args:tt
        accum($match_default:tt; $($m:tt)*)
        input(($($n:tt)*) $($o:tt)*)
    ) => {
        // Move one parenthesized condition `($n*)` onto the list after `$m`.
        $crate::push_rules!(@accum
            args $args
            accum($match_default; $($m)* ($($n)*))
            input($($o)*)
        )
    };

    (@accum
        args $args:tt
        accum $accum:tt
        input($x:tt $($o:tt)*)
    ) => {
        // Anything else is an error
        $crate::respan!($x (compile_error!("expected parenthesized expression or at most one `!`")))
    };
    // Matching phase: peel off the patterns one by one and match them against the values
    // extracted from the input. For anything that is not an identity pattern, invoke `downcast`.

    (@match $conclusion_name:ident inputs() patterns() args(@body ($judgment_name:ident; $n:literal; $v:expr; $output:expr); $inputs:tt; $($m:tt)*)) => {
        tracing::trace_span!("matched rule", rule = $n, judgment = stringify!($judgment_name)).in_scope(|| {
            tracing::debug!("matched rule {:?}", $n);
            #[allow(unused_mut)]
            let mut child_proof_trees: Vec<$crate::judgment::ProofTree> = Vec::new();
            $crate::push_rules!(
                @body ($judgment_name, $n, $v, $output); $inputs; child_proof_trees;
                $($m)*
            );
        });
    };

    (@match $conclusion_name:ident inputs() patterns(,) args $args:tt) => {
        $crate::push_rules!(@match $conclusion_name inputs() patterns() args $args);
    };

    (@match $conclusion_name:ident inputs() patterns ($pattern0:tt $($pattern1:tt)*) args $args:tt) => {
        $crate::respan!($pattern0 (compile_error!("more patterns in rule than arguments on fn")))
    };

    (@match $conclusion_name:ident inputs $inputs:tt patterns() args $args:tt) => {
        $crate::respan!($conclusion_name (compile_error!("fewer patterns in rule than arguments on fn")))
    };

    (@match $conclusion_name:ident inputs($in0:ident $($inputs:tt)*) patterns($pat0:ident : $ty0:ty, $($pats:tt)*) args $args:tt) => {
        {
            if let Some($pat0) = $crate::Downcast::downcast::<$ty0>($in0) {
                $crate::push_rules!(@match $conclusion_name inputs($($inputs)*) patterns($($pats)*) args $args);
            }
        }
    };

    (@match $conclusion_name:ident inputs($in0:ident $($inputs:tt)*) patterns($pat0:ident, $($pats:tt)*) args $args:tt) => {
        {
            let $pat0 = Clone::clone($in0);
            $crate::push_rules!(@match $conclusion_name inputs($($inputs)*) patterns($($pats)*) args $args);
        }
    };

    (@match $conclusion_name:ident inputs($in0:ident $($inputs:tt)*) patterns($pat0:pat, $($pats:tt)*) args $args:tt) => {
        if let Some($pat0) = $crate::Downcast::downcast(&$in0) {
            $crate::push_rules!(@match $conclusion_name inputs($($inputs)*) patterns($($pats)*) args $args);
        }
    };

    // (@match (($arg0:ident @ $pat0:pat) $($args:tt)*) ($n:literal; $v:expr; $output:ident) $($m:tt)*) => {
    //     if let Some($pat0) = $crate::cast::Downcast::downcast(&$arg0) {
    //         $crate::push_rules!(@match ($($args)*) ($n; $v; $output) $($m)*);
    //     }
    // };

    // `@body (v)` phase: processes the conditions, generating the code
    // to evaluate the rule. This is effectively an iterator chain. The
    // expression `v` is carried in from the conclusion and forms the final
    // output of this rule, once all the conditions are evaluated.

    (
        @body $args:tt; ($failed_rules:expr, $match_var:ident, $input_info:tt, $rule_name:literal); $child_proof_trees:ident;
        ! $($m:tt)*
    ) => {
        $match_var = true;
        $crate::push_rules!(@body $args; ($failed_rules, $match_var, $input_info, $rule_name); $child_proof_trees; $($m)*);
    };

    (
        @body $args:tt; $inputs:tt; $child_proof_trees:ident;
        (if let $p:pat = $e:expr) $($m:tt)*
    ) => {
        match $crate::judgment::try_catch(|| Ok($e)) {
            Ok(value) => {
                if let $p = Clone::clone(&value) {
                    $crate::push_rules!(@body $args; $inputs; $child_proof_trees; $($m)*);
                } else {
                    $crate::push_rules!(@record_failure $inputs; $e; $crate::judgment::RuleFailureCause::IfLetDidNotMatch {
                        pattern: stringify!($p).to_string(),
                        value: format!("{:?}", value),
                    });
                }
            }

            Err(e) => {
                $crate::push_rules!(@record_failure $inputs; $e; e);
            }
        }
    };

    // For `(if ...)`, we have special treatment to try and extract the arguments so we can give better information
    // about why the expression evaluated to false.
    (
        @body $args:tt; $inputs:tt; $child_proof_trees:ident;
        (if $($c:tt)*) $($m:tt)*
    ) => {
        $crate::push_rules!(@body_if $args; $inputs; $child_proof_trees; $($c)*; $($c)*; $($m)*)
    };

    (
        @body_if $args:tt; $inputs:tt; $child_proof_trees:ident;
        $arg0:ident . $method:ident ( ); $origcond:expr; $($m:tt)*
    ) => {
        $crate::push_rules!(@structured_if $args; $inputs; $child_proof_trees; (arg0 = $arg0); arg0.$method(); $origcond; $($m)*)
    };

    (
        @body_if $args:tt; $inputs:tt; $child_proof_trees:ident;
        ! $arg0:ident . $method:ident ( ); $origcond:expr; $($m:tt)*
    ) => {
        $crate::push_rules!(@structured_if $args; $inputs; $child_proof_trees; (arg0 = $arg0); !arg0.$method(); $origcond; $($m)*)
    };

    (
        @body_if $args:tt; $inputs:tt; $child_proof_trees:ident;
        $arg0:ident . $method:ident ( $arg1:expr $(,)? ); $origcond:expr; $($m:tt)*
    ) => {
        $crate::push_rules!(@structured_if $args; $inputs; $child_proof_trees; (arg0 = $arg0, arg1 = $arg1); arg0.$method(arg1); $origcond; $($m)*)
    };

    (
        @body_if $args:tt; $inputs:tt; $child_proof_trees:ident;
        ! $arg0:ident . $method:ident ( $arg1:expr $(,)? ); $origcond:expr; $($m:tt)*
    ) => {
        $crate::push_rules!(@structured_if $args; $inputs; $child_proof_trees; (arg0 = $arg0, arg1 = $arg1); !arg0.$method(arg1); $origcond; $($m)*)
    };

    (
        @body_if $args:tt; $inputs:tt; $child_proof_trees:ident;
        $arg0:ident . $method:ident ( $arg1:expr, $arg2:expr $(,)? ); $origcond:expr; $($m:tt)*
    ) => {
        $crate::push_rules!(@structured_if $args; $inputs; $child_proof_trees; (arg0 = $arg0, arg1 = $arg1, arg2 = $arg2); arg0.$method(arg1, arg2); $origcond; $($m)*)
    };

    (
        @body_if $args:tt; $inputs:tt; $child_proof_trees:ident;
        ! $arg0:ident . $method:ident ( $arg1:expr, $arg2:expr $(,)? ); $origcond:expr; $($m:tt)*
    ) => {
        $crate::push_rules!(@structured_if $args; $inputs; $child_proof_trees; (arg0 = $arg0, arg1 = $arg1, arg2 = $arg2); !arg0.$method(arg1, arg2); $origcond; $($m)*)
    };

    (
        @body_if $args:tt; $inputs:tt; $child_proof_trees:ident;
        $func:ident ( $arg0:expr $(,)? ); $origcond:expr; $($m:tt)*
    ) => {
        $crate::push_rules!(@structured_if $args; $inputs; $child_proof_trees; (arg0 = $arg0); $func(arg0); $origcond; $($m)*)
    };

    (
        @body_if $args:tt; $inputs:tt; $child_proof_trees:ident;
        ! $func:ident ( $arg0:expr $(,)? ); $origcond:expr; $($m:tt)*
    ) => {
        $crate::push_rules!(@structured_if $args; $inputs; $child_proof_trees; (arg0 = $arg0); !$func(arg0); $origcond; $($m)*)
    };

    (
        @body_if $args:tt; $inputs:tt; $child_proof_trees:ident;
        $func:ident ( $arg0:expr, $arg1:expr $(,)? ); $origcond:expr; $($m:tt)*
    ) => {
        $crate::push_rules!(@structured_if $args; $inputs; $child_proof_trees; (arg0 = $arg0, arg1 = $arg1); $func(arg0, arg1); $origcond; $($m)*)
    };

    (
        @body_if $args:tt; $inputs:tt; $child_proof_trees:ident;
        ! $func:ident ( $arg0:expr, $arg1:expr $(,)? ); $origcond:expr; $($m:tt)*
    ) => {
        $crate::push_rules!(@structured_if $args; $inputs; $child_proof_trees; (arg0 = $arg0, arg1 = $arg1); ! $func(arg0, arg1); $origcond; $($m)*)
    };

    (
        @body_if $args:tt; $inputs:tt; $child_proof_trees:ident;
        $arg0:ident == $arg1:ident; $origcond:expr; $($m:tt)*
    ) => {
        $crate::push_rules!(@structured_if $args; $inputs; $child_proof_trees; (arg0 = $arg0, arg1 = $arg1); arg0 == arg1; $origcond; $($m)*)
    };

    (
        @body_if $args:tt; $inputs:tt; $child_proof_trees:ident;
        $arg0:ident != $arg1:ident; $origcond:expr; $($m:tt)*
    ) => {
        $crate::push_rules!(@structured_if $args; $inputs; $child_proof_trees; (arg0 = $arg0, arg1 = $arg1); arg0 != arg1; $origcond; $($m)*)
    };

    (
        @body_if $args:tt; $inputs:tt; $child_proof_trees:ident;
        $e:expr; $origcond:expr; $($m:tt)*
    ) => {
        $crate::push_rules!(@structured_if $args; $inputs; $child_proof_trees; (); $e; $origcond; $($m)*)
    };

    (
        @structured_if $args:tt; $inputs:tt; $child_proof_trees:ident;
        ($($argn:ident = $arge:expr),*); $cond:expr; $origcond:expr; $($m:tt)*
    ) => {
        $(
            let $argn = &$arge;
        )*
        let if_then = $crate::judgment::IfThen::new(
            stringify!($origcond),
            vec![
                $(
                    (
                        stringify!($arge),
                        $crate::judgment::DebugString::new(&$argn),
                    ),
                )*
            ],
        );
        if {
            $cond
        } {
            $child_proof_trees.push($crate::judgment::ProofTree::leaf(format!("{if_then:?}")));
            $crate::push_rules!(@body $args; $inputs; $child_proof_trees; $($m)*);
        } else {
            $crate::push_rules!(@record_failure $inputs; $origcond; $crate::judgment::RuleFailureCause::IfFalse(if_then));
        }
    };

    // for_all with accumulator: forward to @body_for_all
    (
        @body $args:tt; $inputs:tt; $child_proof_trees:ident;
        (for_all($loop_var:ident in $collection:expr) with($($with_var:ident),*) $($inner_step:tt)*) $($m:tt)*
    ) => {
        $crate::push_rules!(@body_for_all
            $args; $inputs; $child_proof_trees;
            $loop_var; $collection;
            with($($with_var),*);
            ($($inner_step)*);
            $($m)*);
    };

    // for_all without accumulator: forward to @body_for_all
    (
        @body $args:tt; $inputs:tt; $child_proof_trees:ident;
        (for_all($loop_var:ident in $collection:expr) $($inner_step:tt)*) $($m:tt)*
    ) => {
        $crate::push_rules!(@body_for_all
            $args; $inputs; $child_proof_trees;
            $loop_var; $collection;
            with();
            ($($inner_step)*);
            $($m)*);
    };

    // @body_for_all: shared implementation for for_all with and without accumulators
    (
        @body_for_all
        $args:tt; $inputs:tt; $child_proof_trees:ident;
        $loop_var:ident; $collection:expr;
        with($($with_var:ident),*);
        ($($inner_step:tt)*);
        $($m:tt)*
    ) => {
        {
            let collection = $collection;
            let mut errored = false;

            let (file, line, column) = $crate::respan!($collection ((file!(), line!(), column!())));

            let mut loop_carried = ($($with_var.clone(),)*);

            let mut for_all_proof_trees = vec![];
            for $loop_var in collection {
                if errored {
                    break;
                }

                #[allow(unused)]
                let ($($with_var,)*) = loop_carried.clone();

                let attributes = vec![("item".to_string(), format!("{:?}", $loop_var))];
                let mut item_proof_trees = vec![];
                let mut next_carried = None;
                $crate::push_rules!(@body (loop(next_carried, ($($with_var,)*))); $inputs; item_proof_trees; $($inner_step)*);

                if let Some(next_carried) = next_carried {
                    for_all_proof_trees.push($crate::judgment::ProofTree::with_all(
                        "for_all",
                        attributes,
                        None,
                        &file[..],
                        line,
                        column,
                        item_proof_trees,
                    ));

                    loop_carried = next_carried;
                } else {
                    errored = true;
                }
            }

            if !errored {
                #[allow(unused)]
                let ($($with_var,)*) = loop_carried.clone();

                // All iterations succeeded
                let for_all_tree = $crate::judgment::ProofTree::new(
                    "for_all",
                    None,
                    for_all_proof_trees,
                );
                $child_proof_trees.push(for_all_tree);
                $crate::push_rules!(@body $args; $inputs; $child_proof_trees; $($m)*);
            }
        }
    };

    (
        @body $args:tt; $inputs:tt; $child_proof_trees:ident;
        (assert $c:expr) $($m:tt)*
    ) => {
        assert!($c);
        $crate::push_rules!(@body $args; $inputs; $child_proof_trees; $($m)*);
    };

    // Special case for `(expr => ())` - since `()` is a singleton type, there's at most
    // one successful path. This avoids the closure in `each_proof`, which makes borrowing
    // easier in the judgment body.
    (
        @body $args:tt; $inputs:tt; $child_proof_trees:ident;
        ($i:expr => ()) $($m:tt)*
    ) => {
        match $crate::judgment::CheckProven::check_proven(
            $i,
        ) {
            Ok(proof_tree) => {
                $child_proof_trees.push(proof_tree);
                $crate::push_rules!(@body $args; $inputs; $child_proof_trees; $($m)*);
            }
            Err(e) => {
                $crate::push_rules!(@record_failure $inputs; $i; e);
            }
        }
    };

    (
        @body $args:tt; $inputs:tt; $child_proof_trees:ident;
        ($i:expr => $p:pat) $($m:tt)*
    ) => {
        if let Err(e) = $crate::judgment::EachProof::each_proof(
            $i,
            |($p, proof_tree)| {
                // Remember the size of the child proof tree stack
                let len = $child_proof_trees.len();

                // Push this child proof tree
                $child_proof_trees.push(proof_tree);

                // Recursively process successors
                $crate::push_rules!(@body $args; $inputs; $child_proof_trees; $($m)*);

                // Restore the original child proof tree stack; note that there may be multiple entries
                // in case "non-iterative" steps like `(let ...)` and `(if ...)` are interspersed.
                assert!($child_proof_trees.len() > len);
                $child_proof_trees.truncate(len);
            },
        ) {
            $crate::push_rules!(@record_failure $inputs; $i; e);
        }
    };

    (
        @body $args:tt; $inputs:tt; $child_proof_trees:ident;
        (let $p:ident /*[1]*/: $t:ty = $i:expr) $($m:tt)*
    ) => {
        // [1] I'd prefer to have `$p:pat` but the follow-set rules don't allow for it.
        // That's dumb.
        match $crate::judgment::try_catch::<$t>(|| Ok($i)) {
            Ok(p) => {
                let proof_tree = $crate::judgment::ProofTree::leaf(format!("{} = {p:?}", stringify!($p)));
                let $p = p;
                $child_proof_trees.push(proof_tree);
                $crate::push_rules!(@body $args; $inputs; $child_proof_trees; $($m)*);
            }

            Err(e) => {
                $crate::push_rules!(@record_failure $inputs; $i; e);
            }
        }
    };

    (
        @body $args:tt; $inputs:tt; $child_proof_trees:ident;
        (let $p:pat = $i:expr) $($m:tt)*
    ) => {
        match $crate::judgment::try_catch(|| Ok($i)) {
            Ok(p) => {
                let proof_tree = $crate::judgment::ProofTree::leaf(format!("{} = {p:?}", stringify!($p)));
                let $p = p;
                $child_proof_trees.push(proof_tree);
                $crate::push_rules!(@body $args; $inputs; $child_proof_trees; $($m)*);
            }

            Err(e) => {
                $crate::push_rules!(@record_failure $inputs; $i; e);
            }
        }
    };
    // Special case for `(expr => ())` - since `()` is a singleton type, there's at most
    // one successful path. This avoids the closure in `each_proof`, which makes borrowing
    // easier in the judgment body.
    (
        @body $args:tt; $inputs:tt; $child_proof_trees:ident;
        ($p:pat in $i:expr) $($m:tt)*
    ) => {
        if let Err(e) = $crate::judgment::member_of(
            $i,
            || stringify!($i).to_string(),
            |($p, proof_tree)| {
                // Remember the size of the child proof tree stack
                let len = $child_proof_trees.len();

                // Push this child proof tree
                $child_proof_trees.push(proof_tree);

                // Recursively process successors
                $crate::push_rules!(@body $args; $inputs; $child_proof_trees; $($m)*);

                // Restore the original child proof tree stack; note that there may be multiple entries
                // in case "non-iterative" steps like `(let ...)` and `(if ...)` are interspersed.
                assert!($child_proof_trees.len() > len);
                $child_proof_trees.truncate(len);
            },
        ) {
            $crate::push_rules!(@record_failure $inputs; $i; e);
        }
    };

    (
        @body
            ($judgment_name:ident, $rule_name:literal, $v:expr, $output:expr);
            ($_failed_rules:expr, $_match_var:ident, ($input_judgment_name:expr, $input_attributes:expr), $_rule_name:literal);
            $child_proof_trees:ident;
    ) => {
        {
            let result = $crate::Upcast::upcast($v);
            let mut attributes = $input_attributes.clone();
            attributes.push(("result".to_string(), format!("{:?}", result)));
            let (file, line, column) = $crate::respan!($rule_name ((file!(), line!(), column!())));
            let proof_tree = $crate::judgment::ProofTree::with_all(
                $input_judgment_name,
                attributes,
                Some($rule_name),
                file,
                line,
                column,
                $child_proof_trees.clone(),
            );
            tracing::debug!("produced {:?} from rule {:?} in judgment {:?}", result, $rule_name, stringify!($judgment_name));
            $crate::judgment::insert_smallest_proof(&mut $output, result, proof_tree);
        }
    };

    (
        @body (loop($next_carried:ident, ($($with_var:ident,)*))); $_inputs:tt; $child_proof_trees:ident;
    ) => {
        // when we complete processing a loop with accumulators, capture the final values.
        $next_carried = Some(($($with_var.clone(),)*));
    };

    (@record_failure ($failed_rules:expr, $match_var:expr, $_input_info:tt, $rule_name:literal); $step_expr:expr; $cause:expr) => {
        let file = $crate::respan!($step_expr (file!()));
        let line = $crate::respan!($step_expr (line!()));
        let column = $crate::respan!($step_expr (column!()));
        let cause = $cause;
        if $match_var {
            tracing::debug!(
                "rule {rn} failed because {cause} ({file}:{line}:{column})",
                rn = $rule_name,
                cause = cause,
                file = file,
                line = line,
                column = column,
            );
            $failed_rules.insert(
                $crate::judgment::FailedRule {
                    rule_name: Some($rule_name.to_string()),
                    file: file.to_string(),
                    line: line,
                    column: column,
                    cause,
                }
            );
        } else {
            tracing::trace!(
                "rule {rn} failed because {cause} ({file}:{line}:{column})",
                rn = $rule_name,
                cause = cause,
                file = file,
                line = line,
                column = column,
            );
        }
    }
}

/// Helper function that just calls `f` and returns the value.
/// Used for implementing `judgement_fn` macro to allow expressions to include `?`.
pub fn try_catch<R>(f: impl FnOnce() -> Fallible<R>) -> Result<R, RuleFailureCause> {
    match f() {
        Ok(v) => Ok(v),

        // Kind of dumb that `Inapplicable` only includes a `String` and not an `anyhow::Error`
        // but it's super annoying to package one of those up in the way we want.
        Err(e) => Err(RuleFailureCause::Inapplicable {
            reason: e.to_string(),
        }),
    }
}

/// Wraps a string that should be used verbatim as the debug output.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DebugString {
    text: String,
}

impl DebugString {
    pub fn new<T: std::fmt::Debug>(value: &T) -> Self {
        Self {
            text: format!("{value:?}"),
        }
    }
}

impl std::fmt::Debug for DebugString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct IfThen {
    /// The condition that must be met for the rule to be applicable (stringified).
    cond: &'static str,

    /// Identified Subexpressions and their evaluated values.
    args: Vec<(&'static str, DebugString)>,
}

impl IfThen {
    pub fn new(cond: &'static str, args: Vec<(&'static str, DebugString)>) -> Self {
        Self { cond, args }
    }
}

impl std::fmt::Debug for IfThen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut debug_struct = f.debug_struct("IfThen");
        debug_struct.field("expression", &self.cond);
        for (expr, value) in &self.args {
            debug_struct.field(expr, value);
        }
        debug_struct.finish()
    }
}
