use crate::rust::term;
use expect_test::expect;
use formality_macros::test;

use crate::prove::decls::Program;

use crate::prove::test_util::test_prove;

#[test]
fn outlives_static_gives_any_lifetime() {
    test_prove(
        Program::empty(),
        term("forall<'x, 'y> {'x : 'static} => {'x : 'y}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [!lt_1, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}

#[test]
fn for_all_outlives_gives_static() {
    test_prove(
        Program::empty(),
        term("forall<T> {for<'a> T : 'a} => {T : 'static}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}

#[test]
fn for_all_outlives_gives_any_lifetime() {
    test_prove(
        Program::empty(),
        term("forall<T, 'x> {for<'a> T : 'a} => {T : 'x}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [!ty_1, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}

#[test]
fn outlives_one_lifetime_does_not_give_static() {
    test_prove(
        Program::empty(),
        term("forall<T, 'x> {T : 'x} => {T : 'static}"),
    )
    .assert_err(expect![[r#"
        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !ty_0 : ' static, via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!ty_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !ty_0, b: ' static, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!ty_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}

#[test]
fn multiple_bound_lifetimes_give_static() {
    // Exercise both binder positions, including an unused bound lifetime.
    for assertion in [
        "forall<T> {for<'a, 'b> T : 'a} => {T : 'static}",
        "forall<T> {for<'a, 'b> T : 'b} => {T : 'static}",
    ] {
        test_prove(Program::empty(), term(assertion))
            .assert_ok(expect!["{Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
    }
}

#[test]
fn higher_ranked_outlives_is_transitive() {
    test_prove(
        Program::empty(),
        term("forall<T, 'x> {T : 'x, for<'a> 'x : 'a} => {T : 'static}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [!ty_1, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}

#[test]
fn type_binder_does_not_strengthen_outlives() {
    // 'x is free in the assumption: quantifying over unrelated variables must
    // neither replace 'x with 'static nor panic on the non-lifetime binder.
    let result = test_prove(
        Program::empty(),
        term("forall<T, 'x> {for<'a, U> T : 'x} => {T : 'static}"),
    );
    assert!(!result.is_proven(), "a type binder must not imply 'static");
}

#[test]
fn const_binder_does_not_strengthen_outlives() {
    let result = test_prove(
        Program::empty(),
        term("forall<T, 'x> {for<'a, const N> T : 'x} => {T : 'static}"),
    );
    assert!(!result.is_proven(), "a const binder must not imply 'static");
}
