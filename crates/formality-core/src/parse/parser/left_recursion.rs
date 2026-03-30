//! Support left-recursive grammars. This is basically just a fixed point
//! operation, but we re-implement it to avoid having to return multiple
//! success values, since we know that's not really needed here.
//!
//! This unfortunately requires `type_id` and type erasure via `dyn Any`.
//! This is because we need to be generic over the language `L` and the
//! result type `T` in our `thread_local!` and you can't have generic
//! thread-local values. So we have to erase types.

use std::{any::Any, any::TypeId, cell::RefCell, fmt::Debug, ops::ControlFlow};

use crate::{
    language::Language,
    parse::{parser::Associativity, ParseError, ParseFrame, ParseResult, Scope, SuccessfulParse},
    Set,
};

use super::Precedence;

thread_local! {
    static STACK: RefCell<Vec<StackEntry>> = Default::default()
}

/// A `'static` version of `SuccessfulParse` for storage in type-erased containers.
///
/// `SuccessfulParse<'t, T>` can't be stored as `dyn Any` because of the `'t`
/// lifetime on its `text: &'t str` field. Since `text` is always a suffix of
/// the `start_text` passed to `enter`, we can store just the remaining length
/// and reconstruct the `&'t str` safely when observed.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct StoredParse<T> {
    /// `text.len()` from the original `SuccessfulParse`.
    /// Used to reconstruct `&'t str` via `&start_text[start_text.len() - remaining_len..]`.
    remaining_len: usize,

    /// The precedence of this parse.
    precedence: Precedence,

    /// The parsed value.
    value: T,
}

impl<T: Clone> StoredParse<T> {
    fn from_successful_parse(sp: &SuccessfulParse<'_, T>) -> Self {
        StoredParse {
            remaining_len: sp.text().len(),
            precedence: sp.precedence,
            value: sp.value.clone(),
        }
    }

    fn to_successful_parse<'t>(&self, start_text: &'t str) -> SuccessfulParse<'t, T> {
        let text = &start_text[start_text.len() - self.remaining_len..];
        SuccessfulParse {
            text,
            precedence: self.precedence,
            value: self.value.clone(),
        }
    }
}

/// Tracks an active parse that is taking place.
struct StackEntry {
    /// The scope pointer: we use `()` instead of `Scope<L>`
    scope: *const (),

    /// The starting text: we use `*const` instead of `&'t str`
    start_text: *const str,

    /// Length of start_text, stored separately so `snapshot_nonterminal_stack`
    /// can read it without dereferencing the raw pointer.
    start_text_len: usize,

    current_state: Option<CurrentState>,

    /// The TypeId of the type `T`.
    type_id: TypeId,

    /// The accumulated seed values, type-erased. When `Some`, contains a
    /// `Box<Set<StoredParse<T>>>` erased to `Box<dyn Any>`. The `TypeId`
    /// in `type_id` is used to downcast back to the correct type.
    value: Option<Box<dyn Any>>,

    ///
    observed: bool,

    /// True if, at this parse position, the text can be parsed as
    /// an in-scope variable. When true, `identifier_nonterminal`
    /// will reject parsing the same text as a plain identifier.
    claimed_as_var: bool,

    /// Human-readable name of the nonterminal being parsed (e.g., "Expr", "Stmt").
    /// Used for parse error backtraces.
    nonterminal_name: &'static str,
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
    pub fn new<L, T>(scope: &Scope<L>, start_text: &str, nonterminal_name: &'static str) -> Self
    where
        L: Language,
        T: Clone + 'static,
    {
        Self {
            scope: erase_type(scope),
            current_state: None,
            start_text_len: start_text.len(),
            start_text,
            type_id: TypeId::of::<T>(),
            value: None,
            observed: false,
            claimed_as_var: false,
            nonterminal_name,
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

    /// Returns all accumulated seed values, reconstructing the `'t` lifetime
    /// from `start_text`. The caller filters by precedence.
    fn observe<'t, T>(&mut self, start_text: &'t str) -> Set<SuccessfulParse<'t, T>>
    where
        T: Clone + Ord + 'static,
    {
        assert_eq!(self.type_id, TypeId::of::<T>());
        assert_eq!(self.start_text, start_text as *const str);
        assert!(self.current_state.is_some());

        self.observed = true;

        let Some(boxed) = &self.value else {
            return Set::new();
        };

        let stored_set = boxed.downcast_ref::<Set<StoredParse<T>>>().expect(
            "type mismatch in left-recursion seed storage (TypeId matched but downcast failed)",
        );

        stored_set
            .iter()
            .map(|sp| sp.to_successful_parse(start_text))
            .collect()
    }

    /// Store a snapshot of all accumulated values as type-erased seeds.
    fn store_seeds<T>(&mut self, all_values: &Set<SuccessfulParse<'_, T>>)
    where
        T: Clone + Ord + 'static,
    {
        assert_eq!(self.type_id, TypeId::of::<T>());

        let stored: Set<StoredParse<T>> = all_values
            .iter()
            .map(StoredParse::from_successful_parse)
            .collect();

        self.value = Some(Box::new(stored));
    }
}

pub(super) fn enter<'s, 't, L, T>(
    scope: &'s Scope<L>,
    text: &'t str,
    nonterminal_name: &'static str,
    mut op: impl FnMut(usize) -> ParseResult<'t, T>,
) -> ParseResult<'t, T>
where
    L: Language,
    T: Debug + Clone + Eq + Ord + 'static,
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
                    let all_seeds = entry.observe::<T>(text);
                    tracing::trace!(
                        "found left-recursive stack entry with precedence {:?}, seeds = {:?}",
                        current_precedence,
                        all_seeds
                    );

                    if all_seeds.is_empty() {
                        // Case 1: no previous value (first round).
                        return ControlFlow::Break(Err(ParseError::at(
                            text,
                            format!(
                                "left-recursion on `{}` with no previous value",
                                std::any::type_name::<T>()
                            ),
                        )));
                    }

                    // Filter seeds by precedence compatibility with the current variant.
                    //
                    // For example, given `E = int | E + E | E * E` where `+` is level 1
                    // and `*` is level 2 (both left-associative):
                    //
                    // When parsing the left `E` in `E * E`, a seed like `a + b` (level 1)
                    // is rejected because level 1 < level 2. This prevents `a + b * c`
                    // from being parsed as `(a + b) * c`. But a seed like `a * b` (level 2)
                    // is accepted, allowing `a * b * c` to be `(a * b) * c`.
                    //
                    // For left-associative variants, seeds with >= the variant's level are
                    // accepted (same level OK, enabling `a + b + c` → `(a + b) + c`).
                    // For right or non-associative, seeds must be strictly higher level
                    // (same level rejected, preventing `a + b + c` → `(a + b) + c`).
                    let valid_seeds: Set<SuccessfulParse<'_, T>> = all_seeds
                        .into_iter()
                        .filter(|seed| {
                            let precedence_valid = match current_precedence.associativity {
                                Associativity::Left => {
                                    seed.precedence.level >= current_precedence.level
                                }
                                Associativity::Right | Associativity::None => {
                                    seed.precedence.level > current_precedence.level
                                }
                                Associativity::Both => true,
                            };
                            tracing::trace!(
                                "seed {:?} precedence_valid = {}",
                                seed,
                                precedence_valid,
                            );
                            precedence_valid
                        })
                        .collect();

                    if valid_seeds.is_empty() {
                        return ControlFlow::Break(Err(ParseError::at(
                            text,
                            format!(
                                "left-recursion with no valid seeds \
                                (current variant has precedence {:?})",
                                current_precedence,
                            ),
                        )));
                    }

                    // Return all valid seeds — each will be tried as a left-recursive
                    // reuse via each_nonterminal's iteration over multiple successes.
                    return ControlFlow::Break(Ok(valid_seeds));
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

        stack.push(StackEntry::new::<L, T>(scope, text, nonterminal_name));
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
    // Now we return sets of successful results rather than a single best result.
    // The fixed-point iteration accumulates all successful parses across rounds,
    // using ALL accumulated values as seeds for left-recursion reuse.

    // First round parse is a bit special, because if we get an error here, we can just return immediately,
    // as there is no base case to build from.
    let mut all_values: Set<SuccessfulParse<'t, T>> = match op(min_precedence_level) {
        Ok(v) => v,
        Err(errs) => return Err(errs),
    };

    // Check whether there was recursion to begin with.
    let observed = with_top!(|top| top.observed);
    if !observed {
        return Ok(all_values); // If not, we are done.
    }

    // OK, this is the interesting case. We may be able to get a better parse.
    loop {
        tracing::trace!(
            "reparsing of left-recursive grammar: all_values = {:#?}",
            all_values
        );

        // Store a type-erased snapshot of all accumulated values as seeds.
        // `observe()` will downcast and reconstruct `SuccessfulParse<'t, T>`
        // from the stored `StoredParse<T>` values.
        with_top!(|top| {
            top.store_seeds::<T>(&all_values);
        });

        // Invoke the operation. During `op`, left-recursive calls will observe
        // all accumulated seeds via the stored snapshot.
        let Ok(new_values) = op(min_precedence_level) else {
            return Ok(all_values); // If not, we are done.
        };

        tracing::trace!(
            "left-recursive grammar yielded: new_values = {:?}",
            new_values
        );

        // If all new results already exist in our accumulated set, we've reached
        // a fixed point and can stop.
        if new_values.iter().all(|nv| all_values.contains(nv)) {
            return Ok(all_values); // Fixed point reached.
        }

        // Otherwise, merge new values and try again.
        all_values.extend(new_values);
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

/// Set `claimed_as_var` on the top stack entry.
/// Called from [`Parser::try_claim_as_var`] when the probe determines
/// that the current text can be parsed as an in-scope variable.
///
/// See the "Variables and scope" section of the formality-core book for details.
pub fn set_claimed_as_var() {
    STACK.with_borrow_mut(|stack| {
        let top = stack
            .last_mut()
            .expect("set_claimed_as_var called with empty stack");
        top.claimed_as_var = true;
    });
}

/// Check whether any stack entry at this scope and text position
/// has `claimed_as_var` set. Used by `identifier_nonterminal` and
/// `identifier_re` to reject identifiers that should be parsed as
/// variables instead. Matches on pointer equality for both scope
/// and text, so the flag only applies at the exact parse position
/// where the probe ran.
///
/// See the "Variables and scope" section of the formality-core book for details.
pub(super) fn is_claimed_as_var<L: Language>(scope: &Scope<L>, text: &str) -> bool {
    let scope_ptr: *const () = erase_type(scope);
    let text_ptr: *const str = text;
    STACK.with_borrow(|stack| {
        stack.iter().rev().any(|entry| {
            entry.scope == scope_ptr
                && std::ptr::eq(text_ptr, entry.start_text)
                && entry.claimed_as_var
        })
    })
}

/// Snapshot the current nonterminal parse stack as [`ParseFrame`]s
/// from outermost to innermost.
///
/// Returns an empty vec if the STACK is already mutably borrowed
/// (e.g., when called from within `enter`'s `with_borrow_mut` block).
pub(crate) fn snapshot_nonterminal_stack() -> Vec<ParseFrame> {
    STACK.with(|cell| match cell.try_borrow() {
        Ok(stack) => stack
            .iter()
            .map(|entry| ParseFrame {
                name: entry.nonterminal_name,
                remaining_len: entry.start_text_len,
            })
            .collect(),
        Err(_) => Vec::new(),
    })
}

fn erase_type<T>(s: &T) -> *const () {
    s as *const T as *const ()
}
