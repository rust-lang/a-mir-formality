//! Support left-recursive grammars. This is basically just a fixed point
//! operation, but we re-implement it to avoid having to return multiple
//! success values, since we know that's not really needed here.
//!
//! This unfortunately requires unsafe and even `type_id`. This is because
//! we need to be generic over the language `L` and the result type
//! `T` in our `thread_local!` and you can't have generic thread-local values.
//! So we have to erase types. Annoying!

use std::{any::TypeId, cell::RefCell, fmt::Debug};

use crate::{
    language::Language,
    parse::{ParseError, ParseResult, Scope, SuccessfulParse},
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

#[allow(dead_code)]
pub(super) struct CurrentState {
    pub left_right: LeftRight,
    pub precedence: Precedence,
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
#[allow(dead_code)]
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

    pub fn matches<L, T>(&self, scope: &Scope<L>, start_text: &str) -> bool
    where
        L: Language,
        T: Clone + 'static,
    {
        let scope: *const () = erase_type(scope);
        let start_text: *const str = start_text;
        let type_id = TypeId::of::<T>();
        scope == self.scope && start_text == self.start_text && self.type_id == type_id
    }

    /// UNSAFE: Caller must guarantee that `self.value` pointer is valid.
    pub unsafe fn observe<'t, T>(&mut self, start_text: &'t str) -> ParseResult<'t, T>
    where
        T: Clone + 'static,
    {
        assert_eq!(self.start_text, start_text as *const str);
        assert_eq!(self.type_id, TypeId::of::<T>());
        assert!(
            self.current_state.is_some(),
            "observed a stack frame with no current state (forgot to call `recuse`?)"
        );

        self.observed = true;

        match self.value {
            Some(ptr) => {
                let ptr = ptr as *const SuccessfulParse<'t, T>;
                // UNSAFE: We rely on the caller to entry ptr is valid.
                let ptr = unsafe { &*ptr };
                Ok(ptr.clone())
            }
            None => Err(ParseError::at(
                start_text,
                format!("recursive grammar for `{}`", std::any::type_name::<T>()),
            )),
        }
    }
}

pub fn enter<'s, 't, L, T>(
    scope: &'s Scope<L>,
    text: &'t str,
    mut op: impl FnMut() -> ParseResult<'t, T>,
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
    let previous_result = STACK.with_borrow_mut(|stack| {
        if let Some(entry) = stack
            .iter_mut()
            .find(|entry| entry.matches::<L, T>(scope, text))
        {
            // UNSAFE: We need to justify that `entry.value` will be valid.
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
            unsafe {
                let result = entry.observe::<T>(text);
                tracing::trace!("found left-recursive stack entry, result = {:?}", result);
                Some(result)
            }
        } else {
            stack.push(StackEntry::new::<L, T>(scope, text));
            None
        }
    });
    if let Some(previous_result) = previous_result {
        return previous_result;
    }

    // Access the top stack frame. Use a macro because we don't support closures
    // that are generic over the return type.
    macro_rules! with_top {
        (|$top:ident| $body:expr) => {
            STACK.with_borrow_mut(|stack| {
                let $top = stack.last_mut().unwrap();
                assert!($top.matches::<L, T>(scope, text));
                $body
            })
        };
    }

    // Pop the stack before we return
    final_fn::final_fn!(STACK.with_borrow_mut(|stack| {
        let top = stack.pop().unwrap();
        assert!(top.matches::<L, T>(scope, text));
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
    match op() {
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
        let Ok(value1) = op() else {
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

pub fn recurse<'s, 't, R>(current_state: CurrentState, op: impl FnOnce() -> R) -> R {
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
