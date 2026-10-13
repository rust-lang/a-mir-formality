//! Tests for the "lifetimes equal via mutual outlives" rule of `prove_eq`.

use crate::rust::term;
use expect_test::expect;
use formality_macros::test;
use std::sync::Arc;

use crate::prove::decls::Program;

use crate::prove::test_util::test_prove;

/// `'a = 'static` holds given `'a: 'static`.
#[test]
fn universal_eq_static_given_outlives() {
    test_prove(
        Program::empty(),
        term("forall<'a> {'a: 'static} => {'a = 'static}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [!lt_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}

/// Without `'a: 'static`, `'a = 'static` does not hold (and, with pending
/// outlives disallowed, cannot be deferred to the borrow checker).
#[test]
fn universal_eq_static_unprovable() {
    test_prove(Program::empty(), term("forall<'a> {} => {'a = 'static}"))
        .assert_err(expect![[r#"
            crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: ' static, assumptions: {}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:18:1: no applicable rules for prove_normalize { p: !lt_0, assumptions: {}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: ' static, assumptions: {}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove_normalize.rs:18:1: no applicable rules for prove_normalize { p: ' static, assumptions: {}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}

/// Two universal lifetimes are equal if each outlives the other.
#[test]
fn universals_eq_given_mutual_outlives() {
    test_prove(
        Program::empty(),
        term("forall<'a, 'b> {'a: 'b, 'b: 'a} => {'a = 'b}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [!lt_1, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}

/// Outlives in one direction only is not equality.
#[test]
fn universals_not_eq_given_one_outlives() {
    test_prove(
        Program::empty(),
        term("forall<'a, 'b> {'a: 'b} => {'a = 'b}"),
    )
    .assert_err(expect![[r#"
        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_0 = !lt_1, via: !lt_0 : !lt_1, assumptions: {!lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_1 : !lt_0, via: !lt_0 : !lt_1, assumptions: {!lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: !lt_0, assumptions: {!lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: !lt_0, via: !lt_0 : !lt_1, assumptions: {!lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_1 : !lt_0, via: !lt_0 : !lt_1, assumptions: {!lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_1, b: !lt_0, assumptions: {!lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: !lt_1, via: !lt_0 : !lt_1, assumptions: {!lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}

/// An existential lifetime is still equated by substitution, not by outlives.
#[test]
fn existential_eq_static_by_substitution() {
    test_prove(Program::empty(), term("exists<'a> {} => {'a = 'static}"))
        .assert_ok(expect!["{Constraints { env: Env { variables: [?lt_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?lt_1 => ' static} }}"]);
}

fn static_impl_program() -> Program {
    Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Static where {}"),
            term("impl Static for &'static u32 {}"),
        ])),
        ..Program::empty()
    }
}

/// The motivating case: matching `&'a u32` against `impl Static for &'static u32`
/// requires `'a = 'static`, which holds given `'a: 'static`.
#[test]
fn impl_for_static_ref_matches_given_outlives() {
    test_prove(
        static_impl_program(),
        term("forall<'a> {'a: 'static} => {Static(&'a u32)}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [!lt_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}

/// ...and does not match without it.
#[test]
fn impl_for_static_ref_does_not_match_without_outlives() {
    test_prove(
        static_impl_program(),
        term("forall<'a> {} => {Static(&'a u32)}"),
    )
    .assert_err(expect![[r#"
        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: &!lt_0 u32 = &' static u32, via: Static(&!lt_0 u32), assumptions: {Static(&!lt_0 u32)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: &!lt_0 u32, via: Static(&!lt_0 u32), assumptions: {Static(&!lt_0 u32)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_0 = ' static, via: Static(&!lt_0 u32), assumptions: {Static(&!lt_0 u32)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_0 : ' static, via: Static(&!lt_0 u32), assumptions: {Static(&!lt_0 u32)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: ' static, assumptions: {Static(&!lt_0 u32)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: !lt_0, via: Static(&!lt_0 u32), assumptions: {Static(&!lt_0 u32)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_0 : ' static, via: Static(&!lt_0 u32), assumptions: {Static(&!lt_0 u32)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: ' static, assumptions: {Static(&!lt_0 u32)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: ' static, via: Static(&!lt_0 u32), assumptions: {Static(&!lt_0 u32)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: &' static u32, via: Static(&!lt_0 u32), assumptions: {Static(&!lt_0 u32)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: ' static = !lt_0, via: Static(&!lt_0 u32), assumptions: {Static(&!lt_0 u32)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_0 : ' static, via: Static(&!lt_0 u32), assumptions: {Static(&!lt_0 u32)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: ' static, assumptions: {Static(&!lt_0 u32)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: ' static, via: Static(&!lt_0 u32), assumptions: {Static(&!lt_0 u32)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: !lt_0 : ' static, via: Static(&!lt_0 u32), assumptions: {Static(&!lt_0 u32)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: ' static, assumptions: {Static(&!lt_0 u32)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:54:1: no applicable rules for prove_normalize_via { goal: !lt_0, via: Static(&!lt_0 u32), assumptions: {Static(&!lt_0 u32)}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "trait implied bound" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}
