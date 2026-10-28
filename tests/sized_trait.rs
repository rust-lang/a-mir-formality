//! `Sized` is a trait that can be written in bounds.

use a_mir_formality::{crates, FormalityTest};
use formality_core::test;

#[test]
fn sized_bound_is_provable() {
    FormalityTest::new(crates![crate Foo {
        trait Needs<T> where T: Sized {}
        impl Needs<u32> for u32 {}
    }])
    .skip_execute()
    .ok()
}

#[test]
fn sized_bound_not_met_by_unimplemented_type() {
    FormalityTest::new(crates![crate Foo {
        struct S {}
        trait Needs<T> where T: Sized {}
        impl Needs<S> for u32 {}
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: S = &?lt_0 ?ty_1, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [?lt_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: S, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [?lt_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_0 ?ty_1, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [?lt_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: S = &?lt_0 mut ?ty_1, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [?lt_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: S, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [?lt_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_0 mut ?ty_1, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [?lt_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: S = (), via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: S, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: S = bool, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: S, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: S = i128, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: S, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i128, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: S = i16, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: S, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: S = i32, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: S, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: S = i64, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: S, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: S = i8, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: S, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: S = isize, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: S, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: S = u128, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: S, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u128, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: S = u16, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: S, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: S = u32, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: S, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: S = u64, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: S, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: S = u8, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: S, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: S = usize, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: S, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: Sized(S), assumptions: {Sized(S)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}

#[test]
fn by_value_trait_param_with_sized_bound_compiles_with_rustc() {
    FormalityTest::new(crates![crate Foo {
        fn consume<T>(value: T) -> () { trusted }

        trait Consumer<T> where T: Sized {
            fn consume_value(value: T) -> () { trusted }
        }
    }])
    .skip_execute()
    .rustc_ok()
    .ok()
}
