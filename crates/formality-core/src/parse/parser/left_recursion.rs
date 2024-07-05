//! Support left-recursive grammars. This is basically just a fixed point
//! operation, but we re-implement it to avoid having to return multiple
//! success values, since we know that's not really needed here.
//!
//! This unfortunately requires unsafe and even `type_id`. This is because
//! we need to be generic over the language `L` and the result type
//! `T` in our `thread_local!` and you can't have generic thread-local values.
//! So we have to erase types. Annoying!

use std::{any::TypeId, cell::RefCell, fmt::Debug, ops::ControlFlow};

use crate::{
    language::Language,
    parse::{parser::Associativity, ParseError, ParseResult, Scope, SuccessfulParse},
};

use super::Precedence;

thread_local! {
    static STACK: RefCell<Vec<StackEntry>> = Default::default()
}

/// Tracks an active parse that is taking place.
struct StackEntry {
    /// The scope pointer: we use `()` instead of `Scope<L>`
    scope: *const (),

    /// The starting text: we use `*const` instead of `&'t str`
    start_text: *const str,

    current_state: Option<CurrentState>,

    /// The TypeId of the type `T`.
    type_id: TypeId,

    /// The intermediate value produced. If `Some`, this is a pointer
    /// to a `SuccessfulParse<'t, T>`.
    value: Option<*const ()>,

    ///
    observed: bool,
}

#[derive(Copy, Clone, Debug)]
pub(super) struct CurrentState {
    pub left_right: LeftRight,
    pub precedence: Precedence,
    pub current_text: *const str,
}

/// Determines the kind of recursion the current variant
/// would have if it recursed. For example, given a grammar
/// with a variant
///
/// ```text
/// E = E + E
/// ````
///
/// when `E` recurses, the first `E` is considered `Left`
/// because it occurs before any tokens have been consumed.
/// The second `E` is considered `Right`.
///
/// This terminology is a bit weird if you have three recursions,
/// e.g. `E = E + E + E`. Really we should consider any further
/// recursions as `Other`, I suppose, but I'm too lazy to deal with that
/// right now.
#[derive(Copy, Clone, Debug)]
pub(super) enum LeftRight {
    /// Have not yet consumed any tokens.
    Left,

    /// Consumed some tokens.
    Right,
}

impl StackEntry {
    pub fn new<L, T>(scope: &Scope<L>, start_text: &str) -> Self
    where
        L: Language,
        T: Clone + 'static,
    {
        Self {
            scope: erase_type(scope),
            current_state: None,
            start_text,
            type_id: TypeId::of::<T>(),
            value: None,
            observed: false,
        }
    }

    pub fn matches_start_state<L, T>(&self, scope: &Scope<L>, start_text: &str) -> bool
    where
        L: Language,
        T: Clone + 'static,
    {
        let scope: *const () = erase_type(scope);
        let start_text: *const str = start_text;
        let type_id = TypeId::of::<T>();
        scope == self.scope && std::ptr::eq(start_text, self.start_text) && self.type_id == type_id
    }

    /// True if a call to parse a value of type `T` with the given scope scope/text
    /// matches the current state of this stack frame -- this means that it is a recursive
    /// call from this stack frame (directly or indirectly).
    ///
    /// # Example
    ///
    /// Consider this grammar:
    ///
    /// ```text
    /// E = E + E
    ///   | ( E )
    ///   | integer
    /// ```
    ///
    /// and sample input `"1 + (2 + 3)"`.
    ///
    /// We will start with the variant `E = E + E`. This will recursively try to parse
    /// an `E` two times.
    /// Each time it will invoke [`recurse`][] which will record the current text.
    ///
    /// The first time, the current text will be `"1 + (2 + 3)"`, same as the start text.
    /// When we start parsing `E`, we'll invoke [`enter`][] and see a (left) match.
    ///
    /// Later, after we've parsed `1 + `, we'll try to parse another `E`, but with
    /// the current text `"(2 + 3)"`. We'll again see a (right) match and eventually
    /// reach the variant `E = ( E )`.
    ///
    /// This variant will recurse *again*. But this time the current text is `"2 + 3)"`.
    /// This is not considered a recursive match.
    pub fn matches_current_state<L, T>(
        &self,
        scope: &Scope<L>,
        start_text: &str,
    ) -> Option<CurrentState>
    where
        L: Language,
        T: 'static,
    {
        // Convert incoming scope/text to raw pointers.
        let scope: *const () = erase_type(scope);
        let type_id = TypeId::of::<T>();

        // Scope and type-id must match.
        if scope != self.scope || type_id != self.type_id {
            return None;
        }

        // Start text must match *current* text of this frame.
        let start_text: *const str = start_text;
        let Some(current_state) = &self.current_state else {
            panic!("observed a stack frame with no current state (forgot to call `recuse`?");
        };
        if !std::ptr::eq(start_text, current_state.current_text) {
            return None;
        }

        // OK, we have a match!
        Some(*current_state)
    }

    /// UNSAFE: Caller must guarantee that `self.value` pointer is valid.
    pub unsafe fn observe<'t, T>(&mut self, start_text: &'t str) -> Option<SuccessfulParse<'t, T>>
    where
        T: Clone + 'static,
    {
        assert_eq!(self.type_id, TypeId::of::<T>());
        assert_eq!(self.start_text, start_text); // must be left-recursion
        assert!(self.current_state.is_some());

        self.observed = true;

        let ptr = self.value?;
        let ptr = ptr as *const SuccessfulParse<'t, T>;
        // UNSAFE: We rely on the caller to entry ptr is valid.
        let ptr = unsafe { &*ptr };

        Some(ptr.clone())
    }
}

pub(super) fn enter<'s, 't, L, T>(
    scope: &'s Scope<L>,
    text: &'t str,
    mut op: impl FnMut(usize) -> ParseResult<'t, T>,
) -> ParseResult<'t, T>
where
    L: Language,
    T: Debug + Clone + Eq + 'static,
{
    tracing::trace!(
        "enter<{}>(scope={:?}, text={:?})",
        std::any::type_name::<T>(),
        scope,
        text
    );

    // First check whether we are already parsing this same text in this same scope as this same type.
    let mut min_precedence_level = 0;
    let return_value = STACK.with_borrow_mut(|stack| {
        for entry in stack.iter_mut().rev() {
            match entry.matches_current_state::<L, T>(scope, text) {
                // Keep searching.
                None => (),

                // If this is left-recursion, then we will always return some value, but which value
                // depends on a few factors. Consider `E = int | E + E | E * E` as our example. Because this
                // is left-recursion, we know we are recursively parsing the first `E` in the `E + E`
                // or `E * E` variant.
                //
                // The easy case is where there is no previous result. In that case, we return an error.
                // This consitutes the 'base case' for a recursive grammar.
                // We will only successfully parse the `int` case on that first round, but then we will try again.
                //
                // Otherwise, we have a prevous result for E, and we need to decide whether we can
                // embed it into the variant we are currently parsing. Here we must consider the
                // precedence of the variant we are parsing as well as the precedence level of the value
                // we are attempting to reuse. The general rule is that you can embed higher precedence things
                // into lower precedence things but not vice versa.
                //
                // If the current variant is left associative (as are all variants in our example),
                // then we can accept values with the same precedence level as the variant or higher.
                // So if the variant is `E * E` (precedence level = 2), we would only accept precedence
                // level 2 (`E * E`) or 3 (`int`). If the variant were `E + E`, we could acccept anything.
                //
                // If the current variant is right or none associative, then we can accept values with
                // strictly higher precedence level. So e.g. if `E + E` were level 1 and non-associative,
                // then it would accept only things at level 2 or higher.
                Some(CurrentState {
                    left_right: LeftRight::Left,
                    precedence: current_precedence,
                    ..
                }) => {
                    let previous_result = unsafe {
                        // UNSAFE: See [1] below for justification.
                        entry.observe::<T>(text)
                    };
                    tracing::trace!(
                        "found left-recursive stack entry with precedence {:?}, previous_result = {:?}",
                        current_precedence,
                        previous_result
                    );
                    let Some(previous_result) = previous_result else {
                        // Case 1: no previous value.
                        return ControlFlow::Break(Err(ParseError::at(
                            text,
                            format!(
                                "left-recursion on `{}` with no previous value",
                                std::any::type_name::<T>()
                            ),
                        )));
                    };

                    // If there is a previous value, check the precedence as described above.
                    let precedence_valid = match current_precedence.associativity {
                        Associativity::Left => {
                            previous_result.precedence.level >= current_precedence.level
                        }
                        Associativity::Right | Associativity::None => {
                            previous_result.precedence.level > current_precedence.level
                        }
                        Associativity::Both => true,
                    };
                    tracing::trace!(
                        "precedence_valid = {}",
                        precedence_valid,
                    );
                    if !precedence_valid {
                        return ControlFlow::Break(Err(ParseError::at(
                            text,
                            format!(
                                "left-recursion with invalid precedence \
                                (current variant has precedence {:?}, previous value has level {})",
                                current_precedence, previous_result.precedence.level,
                            ),
                        )));
                    }

                    // Return the previous value.
                    return ControlFlow::Break(Ok(previous_result));

                    // [1] UNSAFE: We need to justify that `entry.value` will be valid.
                    //
                    // Each entry in `stack` corresponds to an active stack frame `F` on this thread
                    // and each entry in `stack` is only mutated by `F`
                    //
                    // The value in `entry.value` will either be `None` (in which case it is valid)
                    // or `Some(p)` where `p` is a pointer.
                    //
                    // `p` will have been assigned by `F` just before invoking `op()`. It is a reference
                    // to the last value in a vector owned by `F`. Since `F` is still active, that vector
                    // is still valid. The borrow to produce `p` is valid (by inspection) because there are no
                    // accesses to the vector until `op` completes
                    // (and, to arrive at this code, `op` has not yet completed).
                }

                // If this is right-recursion, then we will not reuse the previous value, but we will
                // consult the level of the variant to limit the variants we consider in this parse.
                // Consider `E = int | E + E | E * E` as our example. Because this
                // is right-recursion, we know we are recursively parsing the SECOND `E` in the `E + E`
                // or `E * E` variant.
                //
                // If the current variant is left or none associative (as in our example),
                // then we set the minimum precedence level to **one higher** than the variant's precedence.
                // So if `E * E` is the current variant, we would only permit precedence level 2 (the `int` variant).
                // So if `E + E` is the current variant, we would permit precedence level 1 or 2.
                //
                // If the current variant is right associative, then we can accept values with
                // equal precedence.
                Some(CurrentState {
                    left_right: LeftRight::Right,
                    precedence: current_precedence,
                    ..
                }) => {
                    tracing::trace!(
                        "found right-recursive stack entry with precedence {:?}",
                        current_precedence,
                    );
                    match current_precedence.associativity {
                        Associativity::Left | Associativity::None => {
                            min_precedence_level = current_precedence.level + 1;
                        }
                        Associativity::Right => {
                            min_precedence_level = current_precedence.level;
                        }
                        Associativity::Both => {}
                    };
                    break;
                }
            }
        }

        stack.push(StackEntry::new::<L, T>(scope, text));
        ControlFlow::Continue(())
    });

    if let ControlFlow::Break(return_value) = return_value {
        return return_value;
    }
    tracing::trace!("min_precedence_level = {}", min_precedence_level,);

    // Access the top stack frame. Use a macro because we don't support closures
    // that are generic over the return type.
    macro_rules! with_top {
        (|$top:ident| $body:expr) => {
            STACK.with_borrow_mut(|stack| {
                let $top = stack.last_mut().unwrap();
                assert!($top.matches_start_state::<L, T>(scope, text));
                $body
            })
        };
    }

    // Pop the stack before we return
    final_fn::final_fn!(STACK.with_borrow_mut(|stack| {
        let top = stack.pop().unwrap();
        assert!(top.matches_start_state::<L, T>(scope, text));
    }));

    // EXAMPLE: Consider this grammar
    //
    // ```
    // Expr = Expr '+' Expr
    //      | Integer
    // ```
    //
    // and this input `2 + 3`. We process this in rounds.
    //
    // Round 0: Previous value `value` is `None`. When we go to parse expr, it will recurse,
    // which will yield an error that consumes zero tokens. We will then attempt integer,
    // which succeeds, yielding a parsed result of `2` with remainder `+ 3`.
    //
    // Round 1: We store `(2, "+ 3")` as the previous result and try again. When we go to parse `Expr`,
    // there are two options. First, we successfully parse as an integer just like before.
    // But also we are able to parse as `Expr + Expr`, because the left recursive reference to `Expr` yields `2`
    // and we can continue and parse `2 + 3`. The `Parser` prefers this longer result and so we get
    // `2 + 3` as the final result.
    //
    // Round 2: We store `(2+3, "")` as the previous result and try again. *This time* when we recurse,
    // we get `2` again! The reason why is a bit surprising. The parse of `2` succeeds with remainder
    // `"+ 3"`. But when we go parse `Expr + Expr`, the first `Expr` result yields `2 + 3` and there are no more
    // tokens, so that arm fails. In our loop below, we search back through the result and find that `2` has already
    // occurred, so we take `2 + 3` as the best overall parse.
    //
    // It's a bit subtle why this is ok. It's relying on some properties of grammars and parsing.
    // To be more obviously correct we would want to return sets of successful results.
    // In particular, the assumption is that `op` is always returning a best result (if any) and panicking on
    // ambiguity.

    // First round parse is a bit special, because if we get an error here, we can just return immediately,
    // as there is no base case to build from.
    let mut values = vec![];
    match op(min_precedence_level) {
        Ok(v) => values.push(v),
        Err(errs) => return Err(errs),
    };

    // Check whether there was recursion to begin with.
    let observed = with_top!(|top| top.observed);
    if !observed {
        return Ok(values.pop().unwrap()); // If not, we are done.
    }

    // OK, this is the interesting case. We may be able to get a better parse.
    loop {
        tracing::trace!(
            "reparsing of left-recursive grammar: values = {:#?}",
            values
        );

        // If we have an intermediate value, update the stack entry to point at.
        // This takes a borrow of `value` but converts it into a raw pointer.
        // This borrow lasts until after `op` is complete.
        let best_value = values.last().unwrap();
        with_top!(|top| {
            top.value = Some(erase_type(best_value));
        });

        // Invoke the operation. As noted above, if we get a failed parse NOW,
        // we know we already found the best result, so we can just use it.
        let Ok(value1) = op(min_precedence_level) else {
            return Ok(values.pop().unwrap()); // If not, we are done.
        };

        tracing::trace!("left-recursive grammar yielded: value1 = {:?}", value1);

        // If we got back on the previous results we saw, then we're entering
        // a loop and we can stop and take the best one (which should also be the longest).
        // In our example, this occurs when we parse `6` -- the first result
        // succeeds, but we have to try again to see if there's a more complex
        // expression that can be produced (there isn't).
        if values.iter().any(|v| *v == value1) {
            return Ok(values.pop().unwrap()); // If not, we are done.
        }

        // Otherwise, we have to try again.
        values.push(value1);
    }
}

pub(super) fn recurse<R>(current_state: CurrentState, op: impl FnOnce() -> R) -> R {
    STACK.with_borrow_mut(|stack| {
        let top = stack.last_mut().unwrap();
        assert!(
            top.current_state.is_none(),
            "top of stack already has a current state"
        );
        top.current_state = Some(current_state);
    });

    final_fn::final_fn!(STACK.with_borrow_mut(|stack| {
        let top = stack.last_mut().unwrap();
        assert!(
            top.current_state.is_some(),
            "top of stack no longer has a current state"
        );
        top.current_state = None;
    }));

    op()
}

fn erase_type<T>(s: &T) -> *const () {
    s as *const T as *const ()
}
