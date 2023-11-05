use formality_core::term;
use std::sync::Arc;

#[test]
fn precedence() {
    #[term]
    pub enum Root {
        #[cast]
        Id(Id),

        #[grammar($v0 + $v1)]
        Add(Arc<Root>, Arc<Root>),

        #[grammar($v0 * $v1)]
        #[precedence(1)]
        Mul(Arc<Root>, Arc<Root>),
    }

    formality_core::id!(Id);

    let term: Root = crate::ptt::term("a + b * c");
    expect_test::expect![[r#"
        a + b * c
    "#]]
    .assert_debug_eq(&term);
}
