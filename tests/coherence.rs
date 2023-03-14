use formality::test_program_ok;

#[test]
fn test_u32_i32_impls() {
    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
        crate Foo {
            trait Foo<> where [] {}
            impl<> Foo<> for u32 where [] {}
            impl<> Foo<> for i32 where [] {}
        }
    ]",
    ));
}

#[test]
fn test_u32_u32_impls() {
    expect_test::expect![[r#"
        Err(
            "duplicate impl in current crate: impl <> Foo < > for (rigid (scalar u32)) where [] { }",
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
        crate Foo {
            trait Foo<> where [] {}
            impl<> Foo<> for u32 where [] {}
            impl<> Foo<> for u32 where [] {}
        }
    ]",
    ));
}

#[test]
#[allow(non_snake_case)]
fn test_u32_T_impls() {
    expect_test::expect![[r#"
        Err(
            "impls may overlap: `impl <> Foo < > for (rigid (scalar u32)) where [] { }` vs `impl <ty> Foo < > for ^ty0_0 where [] { }`",
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
        crate Foo {
            trait Foo<> where [] {}
            impl<> Foo<> for u32 where [] {}
            impl<ty T> Foo<> for T where [] {}
        }
    ]",
    ));
}

#[test]
#[allow(non_snake_case)]
fn test_u32_T_where_T_Not_impls() {
    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
        crate Foo {
            trait Foo<> where [] {}
            impl<> Foo<> for u32 where [] {}
            impl<ty T> Foo<> for T where [T: Not<>] {}

            trait Not<> where [] {}
        }
    ]",
    ));
}

#[test]
#[allow(non_snake_case)]
fn test_u32_T_where_T_Is_impls() {
    expect_test::expect![[r#"
        Err(
            "impls may overlap: `impl <> Foo < > for (rigid (scalar u32)) where [] { }` vs `impl <ty> Foo < > for ^ty0_0 where [^ty0_0 : Is < >] { }`",
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
        crate Foo {
            trait Foo<> where [] {}
            impl<> Foo<> for u32 where [] {}
            impl<ty T> Foo<> for T where [T: Is<>] {}

            trait Is<> where [] {}
            impl<> Is<> for u32 where [] {}
        }
    ]",
    ));
}
