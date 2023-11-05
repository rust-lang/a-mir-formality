use formality_core::term;
use std::sync::Arc;

#[term]
pub enum Expr {
    #[cast]
    Id(Id),

    #[grammar($v0 + $v1)]
    Add(Arc<Expr>, Arc<Expr>),

    #[grammar($v0 * $v1)]
    #[precedence(1)]
    Mul(Arc<Expr>, Arc<Expr>),
}

formality_core::id!(Id);

#[test]
fn mul_is_higher_precedence() {
    let term: Expr = crate::ptt::term("a + b * c");
    expect_test::expect![[r#"
        a + b * c
    "#]]
    .assert_debug_eq(&term);
}

#[test]
fn equal_precedence_panics() {
    let term: Expr = crate::ptt::term("a + b * c");
    expect_test::expect![[r#"
        a + b * c
    "#]]
    .assert_debug_eq(&term);
}

#[test]
#[should_panic(expected = "extra tokens")]
fn higher_precedence_less_reductions_1() {
    // Subtle: In this case, A has higher precedence,
    // so even though we COULD parse the entire string,
    // we prefer to just parse the first identifier,
    // which results in a panic.
    //
    // In the past, we had a bug in our preference function
    // that caused it to be order dependent, sometimes
    // panicking and sometimes not. Hence we have a similar
    // test with opposite order.

    #[term]
    pub enum Root {
        #[cast]
        #[precedence(1)]
        A(A),

        #[cast]
        B(B),
    }

    #[term($v0)]
    pub struct A(Id);

    #[term($v0 $v1)]
    pub struct B(A, Id);

    formality_core::id!(Id);

    let term: Root = crate::ptt::term("my String");
    expect_test::expect![[r#"
    "#]]
    .assert_debug_eq(&term);
}

#[test]
#[should_panic(expected = "extra tokens")]
fn higher_precedence_less_reductions_2() {
    // Same as `higher_precedence_less_reductions_1` but with
    // opposite term order. See explanation in that function.

    #[term]
    pub enum Root {
        #[cast]
        B(B),

        #[cast]
        #[precedence(1)]
        A(A),
    }

    #[term($v0)]
    pub struct A(Id);

    #[term($v0 $v1)]
    pub struct B(A, Id);

    formality_core::id!(Id);

    let term: Root = crate::ptt::term("my String");
    expect_test::expect![[r#"
        my String
    "#]]
    .assert_debug_eq(&term);
}
