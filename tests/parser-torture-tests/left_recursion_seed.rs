use formality_core::parse::{self, CoreParse};
use formality_core::{term, test};
use std::sync::Arc;

/// Test demonstrating an edge case with left-recursion seed selection.
///
/// The left-recursion fixed-point loop picks the "longest match" (least remaining text)
/// as the seed for each iteration. This test constructs a grammar where the shorter
/// base-case parse (Id) would compose with a left-recursive variant to consume all input,
/// but the longer base-case parse (IdThen) is chosen as seed instead, and it can't compose
/// with anything.
///
/// Grammar:
///   E = E . then . end     // left-recursive: needs ". then . end" after E
///     | $id . then         // base case consuming 2 tokens
///     | $id                // base case consuming 1 token
///
/// Input: `x . then . end`
///
/// Round 0 (no seed, left-recursive variants fail):
///   - `$id . then` matches → IdThen(x), remaining ". end"
///   - `$id` matches         → Id(x),     remaining ". then . end"
///
/// Seed selection: IdThen(x) wins (less remaining text).
///
/// Round 1 (seed = IdThen(x), positioned at ". end"):
///   - E . then . end: reuses IdThen(x), needs ". then . end" but only ". end" left → FAIL
///
/// But if the seed were Id(x), positioned at ". then . end":
///   - E . then . end: reuses Id(x), needs ". then . end", sees ". then . end" → SUCCESS
///     → ThenEnd(Id(x)), consuming all input
///
/// The valid parse ThenEnd(Id(x)) is never discovered.
#[term]
pub enum E {
    #[cast]
    Id(Id),

    #[grammar($v0 . then)]
    IdThen(Id),

    #[grammar($v0 . then . end)]
    ThenEnd(Arc<E>),
}

formality_core::id!(Id);

// FIXME(rust-lang/a-mir-formality#251): The left-recursion loop selects
// only the longest base-case parse as the seed. When a shorter base-case
// parse would compose with a left-recursive variant to produce a valid
// complete parse, that composition is never explored. Here,
// `x . then . end` should parse as ThenEnd(Id(x)) (using Id(x) as the
// left-recursive seed), but IdThen(x) is chosen as the seed instead
// because it consumed more input in round 0. IdThen(x) can't compose
// with any left-recursive variant to consume the remaining `. end`, so
// no complete parse is found.
//
// Fix: try all accumulated values as seeds (not just the longest), or return
// multiple values from left-recursive reuse.
#[test]
fn left_recursion_seed_shadow() {
    let scope: formality_core::parse::Scope<crate::FormalityLang> = Default::default();
    let parses = E::parse(&scope, "x . then . end");

    match parses {
        Ok(parses) => {
            let complete: Vec<_> = parses
                .into_iter()
                .filter(|p| parse::skip_whitespace(p.text()).is_empty())
                .collect();

            // Current (incorrect) behavior: no complete parse is found.
            // The correct result would be ThenEnd(Id(x)).
            assert_eq!(
                complete.len(),
                0,
                "if this starts passing, the seed selection bug may be fixed — \
                 update this test to assert the expected ThenEnd(Id(x)) result"
            );
        }
        Err(_errs) => {
            // Also acceptable for the current buggy behavior — the parse
            // may fail entirely rather than returning incomplete parses.
        }
    }
}

/// Sanity check: the base cases parse correctly on their own.
#[test]
fn base_case_id() {
    let term: E = crate::ptt::term("x");
    expect_test::expect![[r#"
        Id(
            x,
        )
    "#]]
    .assert_debug_eq(&term);
}

/// Sanity check: IdThen parses correctly.
#[test]
fn base_case_id_then() {
    let term: E = crate::ptt::term("x . then");
    expect_test::expect![[r#"
        IdThen(
            x,
        )
    "#]]
    .assert_debug_eq(&term);
}
