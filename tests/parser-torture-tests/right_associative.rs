use formality_core::{term, test};
use std::sync::Arc;

#[term]
pub enum Expr {
    #[cast]
    Id(Id),

    #[grammar($v0 + $v1)]
    #[precedence(1, right)]
    Add(Arc<Expr>, Arc<Expr>),

    #[grammar($v0 * $v1)]
    #[precedence(2, right)]
    Mul(Arc<Expr>, Arc<Expr>),
}

formality_core::id!(Id);

#[test]
fn add_mul() {
    let term: Expr = crate::ptt::term("a + b * c");
    expect_test::expect![[r#"
        Add(
            Id(
                a,
            ),
            Mul(
                Id(
                    b,
                ),
                Id(
                    c,
                ),
            ),
        )
    "#]]
    .assert_debug_eq(&term);
}

#[test]
fn mul_add() {
    let term: Expr = crate::ptt::term("a * b + c");
    expect_test::expect![[r#"
        Add(
            Mul(
                Id(
                    a,
                ),
                Id(
                    b,
                ),
            ),
            Id(
                c,
            ),
        )
    "#]]
    .assert_debug_eq(&term);
}

#[test]
fn add_add() {
    let term: Expr = crate::ptt::term("a + b + c");
    expect_test::expect![[r#"
        Add(
            Id(
                a,
            ),
            Add(
                Id(
                    b,
                ),
                Id(
                    c,
                ),
            ),
        )
    "#]]
    .assert_debug_eq(&term);
}

#[test]
fn mul_mul() {
    let term: Expr = crate::ptt::term("a * b * c");
    expect_test::expect![[r#"
        Mul(
            Id(
                a,
            ),
            Mul(
                Id(
                    b,
                ),
                Id(
                    c,
                ),
            ),
        )
    "#]]
    .assert_debug_eq(&term);
}

#[test]
fn mul_mul_mul() {
    let term: Expr = crate::ptt::term("a * b * c * d");
    expect_test::expect![[r#"
        Mul(
            Id(
                a,
            ),
            Mul(
                Id(
                    b,
                ),
                Mul(
                    Id(
                        c,
                    ),
                    Id(
                        d,
                    ),
                ),
            ),
        )
    "#]]
    .assert_debug_eq(&term);
}

#[test]
fn add_add_mul_add() {
    let term: Expr = crate::ptt::term("a + b + c * d + e");
    expect_test::expect![[r#"
        Add(
            Id(
                a,
            ),
            Add(
                Id(
                    b,
                ),
                Add(
                    Mul(
                        Id(
                            c,
                        ),
                        Id(
                            d,
                        ),
                    ),
                    Id(
                        e,
                    ),
                ),
            ),
        )
    "#]]
    .assert_debug_eq(&term);
}
