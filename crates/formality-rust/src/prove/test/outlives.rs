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
