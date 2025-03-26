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
fn test_effect_fn() {
    let BASE_PROGRAM: &str = "[
        crate test {
            fn foo() -> () where effect is runtime {trusted}
        }
    ]";
    test_where_clause(
        BASE_PROGRAM,
        "{} => {}",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }}"]);
}

// FIXME: this test is incorrect, remove this later
#[test]
fn test_const_trait_unprovable() {
    let base: &str = "[
        crate test {
            const trait Foo {}
        }
    ]";

    test_where_clause(
        base,
        "forall<ty T> { const Foo(T) } => { Foo(T) }",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [!ty_1], bias: Soundness }, known_true: true, substitution: {} }}"]);
}
