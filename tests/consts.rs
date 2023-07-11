use formality::test_program_ok;
use formality_macros::test;

#[test]
fn test_ok() {
    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate Foo {
                trait Foo<const C> where [type_of_const C is bool] {}
                trait Bar<const C> where [type_of_const C is u32] {}

                impl<const C> Foo<const C> for u32 where [type_of_const C is bool] {}
            }
        ]",
    ));
}

#[test]
fn test_holds() {
    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait_impl(impl <> Foo < const 0 > for (rigid (scalar u32)) where [] { })",
                source: "failed to prove {Foo((rigid (scalar u32)), const 0)} given {}, got {}",
            },
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate Foo {
                trait Foo<const C> where [type_of_const C is bool] {}

                impl<> Foo<const true> for u32 where [] {}
            }
        ]",
    ));
}

#[test]
fn test_mismatch() {
    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait_impl(impl <> Foo < const 42 > for (rigid (scalar u32)) where [] { })",
                source: "failed to prove {Foo((rigid (scalar u32)), const 42)} given {}, got {}",
            },
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate Foo {
                trait Foo<const C> where [type_of_const C is bool] {}

                impl<> Foo<const 42> for u32 where [] {}
            }
        ]",
    ));
}
