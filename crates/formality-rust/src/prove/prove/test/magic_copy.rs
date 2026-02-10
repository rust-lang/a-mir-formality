use crate::types::rust::term;
use expect_test::expect;
use formality_macros::test;

use crate::prove::prove::decls::Decls;

use crate::prove::prove::test_util::test_prove;

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Decls {
    Decls {
        trait_decls: vec![
            term("trait Copy<ty Self> where {}"),
            term("trait Magic<ty Self> where {Copy(Self)}"),
        ],
        impl_decls: vec![
            term("impl<ty T> Magic(T) where {Magic(T)}"),
            term("impl Copy(u32) where {}"),
        ],
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
