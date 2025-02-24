//! Test to demonstrate the value of commit points.
//! We distinguish three states when parsing a nonterminal `X`:
//!
//! * Failed completely -- there is no `X` here
//! * Almost succeeded -- there is a `X` here, but it has syntax errors
//! * Succeeded -- there is an `X` here
//!
//! Distinguishing the first two is an art, not a science.
//!
//! A typical parser combinator just distinguishes the first and the last
//! and doesn't have a concept of "almost succeeded", but this makes for
//! significantly worse error messages and less predictable parsing.
//!
//! By default, we say that a parse "almost" succeeds if it consumes
//! any tokens at all. This corresponds to LL(1) grammars. But sometimes
//! it's not good enough!
//!
//! In this test, the `Place` grammar consumes as many `. <id>` projections
//! as it can. But if we consider consuming `.` alone to be enough for a
//! projection to "almost" succeed, we can't parse `$expr.let`. Note that `let`
//! is a keyword, so that is not parsable as a `Place`.
use formality_core::{term, test};
use std::sync::Arc;

#[term]
pub enum Expr {
    #[cast]
    Place(Place),

    #[grammar($v0 . let)]
    Let(Arc<Expr>),
}

#[term($var $*projections)]
pub struct Place {
    var: Id,
    projections: Vec<Projection>,
}

#[term]
pub enum Projection {
    #[grammar(. $v0 $!)]
    Field(Id),
}

formality_core::id!(Id);

/// Check that we can parse `a.b` as a `Field``
#[test]
fn test_parse_field() {
    let e: Expr = crate::ptt::term("a.b");
    expect_test::expect![[r#"
        Place(
            Place {
                var: a,
                projections: [
                    Field(
                        b,
                    ),
                ],
            },
        )
    "#]]
    .assert_debug_eq(&e);
}

/// Check that we can parse `a.let` as a `Let``
#[test]
fn test_parse_let() {
    let e: Expr = crate::ptt::term("a.let");
    expect_test::expect![[r#"
        Let(
            Place(
                Place {
                    var: a,
                    projections: [],
                },
            ),
        )
    "#]]
    .assert_debug_eq(&e);
}
