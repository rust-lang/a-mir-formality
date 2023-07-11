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
        Ok(
            (),
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
                context: "check_trait_impl(impl <> Foo < const 42_(rigid (scalar u32)) > for (rigid (scalar u32)) where [] { })",
                source: "failed to prove {Foo((rigid (scalar u32)), const 42_(rigid (scalar u32)))} given {}, got {}",
            },
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate Foo {
                trait Foo<const C> where [type_of_const C is bool] {}

                impl<> Foo<const 42_u32> for u32 where [] {}
            }
        ]",
    ));
}

#[test]
fn test_generic_mismatch() {
    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait_impl(impl <const> Foo < const ^const0_0 > for (rigid (scalar u32)) where [type_of_const ^const0_0 is (rigid (scalar u32))] { })",
                source: "failed to prove {Foo((rigid (scalar u32)), const !const_1)} given {@ ConstHasType(!const_1 , (rigid (scalar u32)))}, got {}",
            },
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate Foo {
                trait Foo<const C> where [type_of_const C is bool] {}

                impl<const C> Foo<const C> for u32 where [type_of_const C is u32] {}
            }
        ]",
    ));
}

#[test]
fn test_rigid_const_bound() {
    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate Foo {
                trait Foo<> where [type_of_const true is bool] {}
            }
        ]",
    ));
}

#[test]
fn test_nonsense_rigid_const_bound() {
    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait(Foo)",
                source: Error {
                    context: "prove_where_clause_well_formed(type_of_const 0_(rigid (scalar bool)) is (rigid (scalar u32)))",
                    source: "failed to prove {(rigid (scalar u32)) = (rigid (scalar bool))} given {@ ConstHasType(0_(rigid (scalar bool)) , (rigid (scalar u32)))}, got {}",
                },
            },
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate Foo {
                trait Foo<> where [type_of_const true is u32] {}
            }
        ]",
    ));
}

#[test]
fn test_multiple_type_of_const() {
    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate Foo {
                trait Foo<const C> where [type_of_const C is bool, type_of_const C is u32] {}
            }
        ]",
    ));
}
