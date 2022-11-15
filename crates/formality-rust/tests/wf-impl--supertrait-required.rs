#![allow(non_snake_case)]

#[test]
fn test_one_impl() {
    const PROGRAM: &str = "[
        crate core {
            trait Eq<> where [Self: PartialEq<>] { }
            trait PartialEq<> where [] { }
            impl<> Eq<> for u32 where [] { }
        }
    ]";

    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait_impl(impl <> Eq((rigid (scalar u32))) where [] { })",
                source: "could not prove `is_implemented(Eq((rigid (scalar u32))))` given `[]`",
            },
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_program_ok(PROGRAM));
}

#[test]
fn test_both_impls() {
    const PROGRAM: &str = "[
        crate core {
            trait Eq<> where [Self: PartialEq<>] { }
            trait PartialEq<> where [] { }
            impl<> Eq<> for u32 where [] { }
            impl<> PartialEq<> for u32 where [] { }
        }
    ]";

    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_program_ok(PROGRAM));
}
