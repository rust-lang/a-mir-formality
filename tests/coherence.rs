#![allow(non_snake_case)]

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
