use crate::grammar::{Wc, Wcs};
use crate::rust::term;
use expect_test::expect;
use formality_macros::test;

use crate::prove::prove::{decls::Decls, prove::prove};

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Decls {
    Decls {
        program: Decls::program_from_items(vec![
            term("trait Eq where Self : PartialEq {}"),
            term("trait PartialEq where {}"),
        ]),
        ..Decls::empty()
    }
}

#[test]
fn eq_implies_partial_eq() {
    let assumptions: Wcs = Wcs::t();
    let goal: Wc = term("for<T> if {Eq(T)} PartialEq(T)");
    let constraints = prove(decls(), (), assumptions, goal);
    constraints.assert_ok(
    expect!["{Constraints { env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}

#[test]
fn not_partial_eq_implies_eq() {
    let goal: Wc = term("for<T> if {PartialEq(T)} Eq(T)");
    prove(decls(), (), (), goal)
    .assert_err(
    expect![[r#"
        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Eq(!ty_1), via: PartialEq(!ty_1), assumptions: {PartialEq(!ty_1)}, env: Env { variables: [!ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Eq(!ty_1), via: PartialEq(?ty_2), assumptions: {PartialEq(!ty_1)}, env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}

#[test]
fn universals_not_eq() {
    let goal: Wc = term("for<T, U> if {Eq(T)} PartialEq(U)");
    prove(decls(), (), (), goal)
    .assert_err(
    expect![[r#"
        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: PartialEq(!ty_2), via: Eq(!ty_1), assumptions: {Eq(!ty_1)}, env: Env { variables: [!ty_1, !ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 = !ty_1, via: Eq(!ty_0), assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: Eq(!ty_0), assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !ty_1, via: Eq(!ty_0), assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Eq(!ty_1), via: PartialEq(?ty_2), assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}
