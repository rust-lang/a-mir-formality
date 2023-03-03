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

/// Test that `exists<T> is_implemented(Foo(U))` yields `U = Vec<X>` for some fresh `X`
#[test]
fn exists_u_for_t() {
    let program: Program = term(PROGRAM);
    let constraints = test_prove(&program, term("<ty U> ({}, {is_implemented(Foo(U))})"));
    expect![[r#"
        {
            <ty> Constraints { known_true: true, substitution: Substitution { map: {?ty_0: (rigid (adt Vec) ^ty0_0)} } },
        }
    "#]]
        .assert_debug_eq(&constraints);
}
