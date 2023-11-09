#![cfg(FIXME)]
#![allow(non_snake_case)]

#[test]
#[ignore]
fn test_one_impl() {
    const PROGRAM: &str = "[
        crate core {
            trait Eq where Self: PartialEq { }
            trait PartialEq { }
            impl Eq for u32 { }
        }
    ]";

    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait_impl(impl <> Eq((rigid (scalar u32))) { })",
                source: "could not prove `is_implemented(Eq((rigid (scalar u32))))` given `[]`",
            },
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_program_ok(PROGRAM));
}

#[test]
#[ignore]
fn test_both_impls() {
    const PROGRAM: &str = "[
        crate core {
            trait Eq where Self: PartialEq { }
            trait PartialEq { }
            impl Eq for u32 { }
            impl PartialEq for u32 { }
        }
    ]";

    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_program_ok(PROGRAM));
}
