use formality::test_program_ok;

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
                trait Foo<const(bool) C> where [] {}
                trait Bar<const(u32) C> where [] {}

                impl<const(bool) C> Foo<const C> for u32 where [] {}
            }
        ]",
    ));
}

#[test]
fn test_holds() {
    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate Foo {
                trait Foo<const(bool) C> where [] {}

                impl<> Foo<const true> for u32 where [] {}
            }
        ]",
    ));
}
