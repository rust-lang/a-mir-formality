use expect_test::expect;
use formality_macros::test;
use formality_types::parse::term;

use crate::program::Program;

use super::test_prove;

/// Simple example program consisting only of two trait declarations.
fn program() -> Program {
    Program {
        max_size: Program::DEFAULT_MAX_SIZE,
        trait_decls: vec![term("trait Foo<ty Self> where {}")],
        impl_decls: vec![term("impl<ty T> Foo(Vec<T>) where {}")],
        alias_eq_decls: vec![],
        alias_bound_decls: vec![],
    }
}

/// Test that `X = Vec<X>` cannot be solved
#[test]
fn direct_cycle() {
    let constraints = test_prove(program(), term("<ty A> ({}, {equals(A, Vec<A>)})"));
    expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&constraints);
}

/// Test that `X = Vec<X>` cannot be solved (when constructed over several steps)
#[test]
fn indirect_cycle_1() {
    let constraints = test_prove(
        program(),
        term("<ty A, ty B> ({}, {equals(A, Vec<B>), equals(B, A)})"),
    );
    expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&constraints);
}

/// Test that `X = Vec<X>` cannot be solved (when constructed over several steps)
#[test]
fn indirect_cycle_2() {
    let constraints = test_prove(
        program(),
        term("<ty A, ty B> ({}, {equals(B, A), equals(A, Vec<B>)})"),
    );
    expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&constraints);
}
