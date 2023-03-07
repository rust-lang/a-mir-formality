use expect_test::expect;
use formality_macros::test;
use formality_types::parse::term;

use crate::program::Program;

use super::test_prove;

#[test]
fn test_a() {
    let constraints = test_prove(
        Program::empty(),
        term("<> ({}, {for<ty T, ty U> if {T = u32, U = Vec<T>} U = Vec<u32>})"),
    );
    expect![[r#"
        {
            (
                Env {
                    variables: [],
                },
                Constraints {
                    result: (),
                    known_true: true,
                    substitution: {},
                },
            ),
        }
    "#]]
    .assert_debug_eq(&constraints);
}

#[test]
fn test_b() {
    let constraints = test_prove(
        Program::empty(),
        term("<ty A> ({}, {for<ty T, ty U> if {T = u32, U = Vec<T>} A = U})"),
    );
    expect![[r#"
        {}
    "#]] // FIXME
    .assert_debug_eq(&constraints);
}
