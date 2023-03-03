use expect_test::expect;
use formality_macros::test;
use formality_types::parse::term;

use crate::program::Program;

use super::test_prove;

/// Simple example program consisting only of two trait declarations.
const PROGRAM: &str = "
    program(
        [trait Foo<ty Self> where {}],
        [impl<ty T> Foo(Vec<T>) where {}],
        [],
        []
    )
";

/// Test that `X = Vec<X>` cannot be solved
#[test]
fn direct_cycle() {
    let program: Program = term(PROGRAM);
    let constraints = test_prove(&program, term("<ty A> ({}, {equals(A, Vec<A>)})"));
    expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&constraints);
}

/// Test that `X = Vec<X>` cannot be solved (when constructed over several steps)
#[test]
fn indirect_cycle_1() {
    let program: Program = term(PROGRAM);
    let constraints = test_prove(
        &program,
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
    let program: Program = term(PROGRAM);
    let constraints = test_prove(
        &program,
        term("<ty A, ty B> ({}, {equals(B, A), equals(A, Vec<B>)})"),
    );
    expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&constraints);
}
