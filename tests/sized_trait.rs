//! `Sized` is a trait that can be written in bounds.

use a_mir_formality::{crates, FormalityTest};
use formality_core::test;

#[test]
fn sized_bound_is_provable() {
    FormalityTest::new(crates![crate Foo {
        trait Needs<T> where T: Sized {}
        impl Needs<u32> for u32 {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok()
}

// FIXME: ADT Sized support is deferred; rustc accepts this empty struct.
// This test records the current limitation, not the intended Sized semantics.
#[test]
fn sized_bound_not_met_by_unimplemented_type() {
    FormalityTest::new(crates![crate Foo {
        struct S {}
        trait Needs<T> where T: Sized {}
        impl Needs<S> for u32 {}
    }])
    .rustc_ok()
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
        fn consume<T>(value: T) -> () where T: Sized {}

        trait Consumer<T> where T: Sized {
            fn consume_value(value: T) -> () {}
        }
        fn main() -> () { consume::<u32>(22_u32); }
    }])
    .rustc_ok()
    .ok()
}

#[test]
fn shared_reference_is_sized_without_pointee_bound() {
    FormalityTest::new(crates![crate Foo {
        trait Needs<T> where T: Sized {}
        impl<'a, T> Needs<&'a T> for u32 where T: 'a {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

#[test]
fn mutable_reference_is_sized_without_pointee_bound() {
    FormalityTest::new(crates![crate Foo {
        trait Needs<T> where T: Sized {}
        impl<'a, T> Needs<&'a mut T> for u32 where T: 'a {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

#[test]
fn generic_sized_bound_is_forwarded() {
    FormalityTest::new(crates![crate Foo {
        trait Needs<T> where T: Sized {}
        impl<T> Needs<T> for u32 where T: Sized {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}

#[test]
fn generic_parameter_requires_explicit_sized_bound() {
    // Generated Rust still gives T an implicit Sized bound until #500 lands.
    // Formality must already require an explicit bound on this impl.
    FormalityTest::new(crates![crate Foo {
        trait Needs<T> where T: Sized {}
        impl<T> Needs<T> for u32 {}
    }])
    .rustc_ok()
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = &?lt_1 ?ty_2, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0, ?lt_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0, ?lt_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_1 ?ty_2, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0, ?lt_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = &?lt_1 mut ?ty_2, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0, ?lt_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0, ?lt_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: &?lt_1 mut ?ty_2, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0, ?lt_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = (), via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: (), via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = bool, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = i128, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i128, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = i16, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i16, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = i32, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i32, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = i64, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i64, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = i8, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: i8, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = isize, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: isize, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = u128, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u128, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = u16, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u16, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = u32, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = u64, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = u8, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u8, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = usize, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: usize, via: Sized(!ty_0), assumptions: {Sized(!ty_0)}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}

// FIXME(#522): Model Sized with built-in solver rules and reject user-supplied impls.
#[test]
#[ignore = "FIXME(#522): user-supplied Sized impls are not yet rejected"]
fn user_defined_sized_impl_is_rejected() {
    // The pinned rustc rejects this with E0322. Assert rejection without
    // prescribing the diagnostic of the not-yet-implemented check.
    let program = crates![crate Foo {
        struct S {}
        impl Sized for S {}
        fn main() -> () {}
    }];
    assert!(
        a_mir_formality::test_program_ok(&program).is_err(),
        "user-written Sized impls must be rejected, as rustc does with E0322"
    );
}
