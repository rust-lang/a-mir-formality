#![allow(non_snake_case)] // we embed type names into the names for our test functions

use a_mir_formality::{test_program_ok, test_where_clause};
use formality_core::test_util::ResultTestExt;
use formality_macros::test;

#[test]
fn test_const_syntax() {
    let gen_program = |addl: &str| {
        const BASE_PROGRAM: &str = "[
        crate core {
           const trait Default {
           }

           impl const Default for () {
           }
        }

        ]";

        BASE_PROGRAM.replace("ADDITIONAL", addl)
    };

    test_program_ok(&gen_program("")).assert_ok(expect_test::expect!["()"]);
}

#[test]
fn test_runtime_fn_with_runtime_effect() {
    let BASE_PROGRAM: &str = "[
        crate test {
            fn foo() -> () do runtime {(runtime)}
        }
    ]";
    test_where_clause(
        BASE_PROGRAM,
        "{} => {}",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }}"]);
}

#[test]
fn test_const_fn_with_runtime_effect() {
    let BASE_PROGRAM: &str = "[
        crate test {
            fn foo() -> () do const {(runtime)}
        }
    ]";
    test_where_clause(
        BASE_PROGRAM,
        "{} => {}",
    )
    .assert_err(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }}"]);
}
