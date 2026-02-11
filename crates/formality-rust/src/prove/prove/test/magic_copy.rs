use crate::rust::term;
use expect_test::expect;
use formality_macros::test;

use crate::prove::prove::decls::Decls;

use crate::prove::prove::test_util::test_prove;

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Decls {
    Decls {
        program: Decls::program_from_items(vec![
            term("trait Copy where {}"),
            term("trait Magic where Self : Copy {}"),
            term("impl<ty T> Magic for T where T : Magic {}"),
            term("impl Copy for u32 {}"),
        ]),
        ..Decls::empty()
    }
}

#[test]
fn all_t_not_magic() {
    test_prove(decls(), term("{} => {for<ty T> Magic(T)}")).assert_err(
    expect![[r#"
        the rule "symmetric" at (prove_eq.rs) failed because
          cyclic proof attempt: `prove_eq { a: !ty_0, b: u32, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }`

        failed at (prove.rs) because
          cyclic proof attempt: `prove_wc_list { goals: {Copy(!ty_0)}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }`"#]]);
}

#[test]
fn all_t_not_copy() {
    test_prove(decls(), term("{} => {for<ty T> Copy(T)}")).assert_err(
    expect![[r#"
        the rule "symmetric" at (prove_eq.rs) failed because
          cyclic proof attempt: `prove_eq { a: !ty_0, b: u32, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }`

        the rule "symmetric" at (prove_eq.rs) failed because
          cyclic proof attempt: `prove_eq { a: !ty_0, b: u32, assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }`

        failed at (prove.rs) because
          cyclic proof attempt: `prove_wc_list { goals: {Magic(!ty_0)}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }`"#]]);
}
