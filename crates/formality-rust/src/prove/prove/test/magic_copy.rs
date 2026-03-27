use crate::rust::term;
use expect_test::expect;
use formality_macros::test;

use crate::prove::prove::decls::Decls;

use crate::prove::prove::test_util::test_prove;

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Decls {
    Decls {
        crates: Decls::program_from_items(vec![
            term("trait Copy where {}"),
            term("trait Magic where Self : Copy {}"),
            term("impl<T> Magic for T where T : Magic {}"),
            term("impl Copy for u32 {}"),
        ]),
        ..Decls::empty()
    }
}

#[test]
fn all_t_not_magic() {
    test_prove(decls(), term("{} => {for<T> Magic(T)}")).assert_err(
    expect![[r#"
        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = u32, via: Copy(!ty_0), assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Copy(!ty_0), assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: Copy(!ty_0), assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Magic(!ty_0), via: Copy(?ty_1), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Magic(!ty_1), via: Copy(?ty_2), assumptions: {}, env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}

#[test]
fn all_t_not_copy() {
    test_prove(decls(), term("{} => {for<T> Copy(T)}")).assert_err(
    expect![[r#"
        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = u32, via: Copy(!ty_0), assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Copy(!ty_0), assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: Copy(!ty_0), assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = u32, via: Copy(!ty_0), assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Copy(!ty_0), assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: Copy(!ty_0), assumptions: {Copy(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Magic(!ty_0), via: Copy(?ty_1), assumptions: {}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}
