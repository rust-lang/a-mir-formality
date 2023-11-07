use formality_core::term;
use std::sync::Arc;

#[term]
pub enum Expr {
    #[cast]
    Id(Id),

    #[grammar($v0 + $v1)]
    #[precedence(1)]
    Add(Arc<Expr>, Arc<Expr>),

    #[grammar($v0 * $v1)]
    #[precedence(2)]
    Mul(Arc<Expr>, Arc<Expr>),
}

formality_core::id!(Id);

#[test]
fn mul_is_higher_precedence() {
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
// FIXME #[should_panic(expected = "ambiguous parse")]
fn equal_precedence_panics() {
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
