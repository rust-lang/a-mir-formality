use expect_test::expect;
use formality_macros::test;
use formality_types::parse::term;

use crate::program::Program;

/// Simple example program consisting only of two trait declarations.
const PROGRAM: &str = "
    program(
        [],
        [],
        [],
        []
    )
";

/// There is no U that is equal to all T.
#[test]
fn exists_u_for_t() {
    let program: Program = term(PROGRAM);

    let constraints = super::test_prove(&program, term("<ty U> ({}, {for<ty T> equals(T, U)})"));
    expect![[r#"
            {}
        "#]]
    .assert_debug_eq(&constraints);
}
