//! Tests for leaving the scope of a binder: a where-clause pending on a
//! variable is restated when the variable goes out of scope.

use crate::rust::term;
use expect_test::expect;
use formality_macros::test;
use std::sync::Arc;

use crate::prove::decls::Program;

use crate::prove::test_util::test_prove_pending_outlives;

/// `for<'a> ('x: 'a)` iff `'x: 'static`, which is left pending in turn.
#[test]
fn outlives_every_lifetime() {
    test_prove_pending_outlives(Program::empty(), term("exists<'x> {} => {for<'a> 'x : 'a}"))
        .assert_ok(expect!["{Constraints { env: Env { variables: [?lt_1], bias: Soundness, pending: [?lt_1 : ' static], allow_pending_outlives: true }, known_true: true, substitution: {} }}"]);
}

/// ...or is proven, where the assumptions allow.
#[test]
fn outlives_every_lifetime_given_static() {
    test_prove_pending_outlives(
        Program::empty(),
        term("forall<'x> {'x : 'static} => {for<'a> 'x : 'a}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [!lt_1], bias: Soundness, pending: [], allow_pending_outlives: true }, known_true: true, substitution: {} }, Constraints { env: Env { variables: [!lt_1], bias: Soundness, pending: [!lt_1 : ' static], allow_pending_outlives: true }, known_true: true, substitution: {} }}"]);
}

/// `for<'a> ('a: 'x)` holds for no `'x`.
#[test]
fn outlived_by_every_lifetime() {
    test_prove_pending_outlives(Program::empty(), term("exists<'x> {} => {for<'a> 'a : 'x}"))
        .assert_err(expect![[r#"
            failed at (proven_set.rs) because
              `!lt_1 : ?lt_0` cannot be restated without `!lt_1`"#]]);
}

/// Nor for `'static`.
#[test]
fn every_lifetime_outlives_static() {
    test_prove_pending_outlives(Program::empty(), term("{} => {for<'a> 'a : 'static}")).assert_err(
        expect![[r#"
            failed at (proven_set.rs) because
              `!lt_1 : ' static` cannot be restated without `!lt_1`"#]],
    );
}

/// An impl whose lifetime `'b` no parameter of the trait determines.
fn unconstrained_impl_lifetime() -> Program {
    Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Between<'a, 'c> where {}"),
            term("impl<'a, 'b, 'c> Between<'a, 'c> for u32 where 'a : 'b, 'b : 'c {}"),
        ])),
        ..Program::empty()
    }
}

/// `exists<'b> ('x: 'b, 'b: 'y)` iff `'x: 'y`.
#[test]
fn outlives_through_some_lifetime() {
    test_prove_pending_outlives(
        unconstrained_impl_lifetime(),
        term("forall<'x, 'y> {} => {Between(u32, 'x, 'y)}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [!lt_1, !lt_2], bias: Soundness, pending: [!lt_1 : !lt_2], allow_pending_outlives: true }, known_true: true, substitution: {} }}"]);
}
