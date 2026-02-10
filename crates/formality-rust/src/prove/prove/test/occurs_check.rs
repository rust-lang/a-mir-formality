use crate::types::rust::term;
use expect_test::expect;
use formality_macros::test;

use crate::prove::prove::decls::Decls;

use crate::prove::prove::test_util::test_prove;

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Decls {
    Decls {
        trait_decls: vec![term("trait Foo<ty Self> where {}")],
        impl_decls: vec![term("impl<ty T> Foo(Vec<T>) where {}")],
        ..Decls::empty()
    }
}

/// Test that `X = Vec<X>` cannot be solved
#[test]
fn direct_cycle() {
    test_prove(decls(), term("exists<ty A> {} => {A = Vec<A>}")).assert_err(expect![[r#"
                failed at (prove_eq.rs) because
                  `?ty_0` occurs in `Vec<?ty_0>`"#]]);
}

/// Test that `X = Vec<Y>` can be solved
#[test]
fn eq_variable_to_rigid() {
    test_prove(decls(), term("exists<ty X, ty Y> {} => {X = Vec<Y>}")).assert_ok(expect!["{Constraints { env: Env { variables: [?ty_3, ?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_1 => Vec<?ty_3>, ?ty_2 => ?ty_3} }}"]);
}

/// Test that `Vec<Y> = X` can be solved
#[test]
fn eq_rigid_to_variable() {
    test_prove(decls(), term("exists<ty X, ty Y> {} => {Vec<Y> = X}")).assert_ok(expect!["{Constraints { env: Env { variables: [?ty_3, ?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_1 => Vec<?ty_3>, ?ty_2 => ?ty_3} }}"]);
}

/// Test that `X = Vec<X>` cannot be solved (when constructed over several steps)
#[test]
fn indirect_cycle_1() {
    test_prove(
        decls(),
        term("exists<ty A, ty B> {} => {A = Vec<B>, B = A}"),
    )
    .assert_err(expect![[r#"
        failed at (prove_eq.rs) because
          `?ty_0` occurs in `Vec<?ty_0>`"#]]);
}

/// Test that `X = Vec<X>` cannot be solved (when constructed over several steps)
#[test]
fn indirect_cycle_2() {
    test_prove(
        decls(),
        term("exists<ty A, ty B> {} => {B = A, A = Vec<B>}"),
    )
    .assert_err(expect![[r#"
        failed at (prove_eq.rs) because
          `?ty_0` occurs in `Vec<?ty_0>`"#]]);
}
