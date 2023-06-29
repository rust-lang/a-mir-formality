#![allow(non_snake_case)] // we embed type names into the names for our test functions

use formality::test_program_ok;
use formality_macros::test;

#[test]
fn test_orphan_CoreTrait_for_CoreStruct_in_Foo() {
    expect_test::expect![[r#"
        Err(
            Error {
                context: "orphan_check( impl <> CoreTrait < > for (rigid (adt CoreStruct)) where [] { })",
                source: "failed to prove {@ IsLocal(CoreTrait((rigid (adt CoreStruct))))} given {}, got {}",
            },
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                trait CoreTrait<> where [] {}
                struct CoreStruct<> where [] {}
            },
            crate foo {
                impl<> CoreTrait<> for CoreStruct<> where [] {}
            }
        ]",
    ));
}

#[test]
fn test_orphan_neg_CoreTrait_for_CoreStruct_in_Foo() {
    expect_test::expect![[r#"
        Err(
            Error {
                context: "orphan_check_neg( impl <> ! CoreTrait < > for (rigid (adt CoreStruct)) where [] {})",
                source: "failed to prove {@ IsLocal(CoreTrait((rigid (adt CoreStruct))))} given {}, got {}",
            },
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                trait CoreTrait<> where [] {}
                struct CoreStruct<> where [] {}
            },
            crate foo {
                impl<> !CoreTrait<> for CoreStruct<> where [] {}
            }
        ]",
    ));
}

#[test]
fn test_orphan_mirror_CoreStruct() {
    expect_test::expect![[r#"
        Err(
            Error {
                context: "orphan_check( impl <> CoreTrait < > for (alias (Mirror :: Assoc) (rigid (adt CoreStruct))) where [] { })",
                source: "failed to prove {@ IsLocal(CoreTrait((alias (Mirror :: Assoc) (rigid (adt CoreStruct)))))} given {}, got {}",
            },
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                trait CoreTrait<> where [] {}
                struct CoreStruct<> where [] {}

                trait Mirror<> where [] {
                    type Assoc<> : [] where [];
                }
        
                impl<ty T> Mirror<> for T where [] {
                    type Assoc<> = T where [];
                }
            },
            crate foo {
                impl<> CoreTrait<> for <CoreStruct<> as Mirror<>>::Assoc<> where [] {}
            }
        ]",
    ));
}

#[test]
fn test_orphan_mirror_FooStruct() {
    // We are able to normalize and see that the result is local.
    //
    // NB: rustc doesn't do this.
    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                trait CoreTrait<> where [] {}

                trait Mirror<> where [] {
                    type Assoc<> : [] where [];
                }
        
                impl<ty T> Mirror<> for T where [] {
                    type Assoc<> = T where [];
                }
            },
            crate foo {
                struct FooStruct<> where [] {}
                impl<> CoreTrait<> for <FooStruct<> as Mirror<>>::Assoc<> where [] {}
            }
        ]",
    ));
}

#[test]
fn test_orphan_alias_to_unit() {
    // We are able to normalize and see that the result is local.
    //
    // NB: rustc doesn't do this.
    expect_test::expect![[r#"
        Err(
            Error {
                context: "orphan_check( impl <> CoreTrait < > for (alias (Unit :: Assoc) (rigid (adt FooStruct))) where [] { })",
                source: "failed to prove {@ IsLocal(CoreTrait((alias (Unit :: Assoc) (rigid (adt FooStruct)))))} given {}, got {}",
            },
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                trait CoreTrait<> where [] {}

                trait Unit<> where [] {
                    type Assoc<> : [] where [];
                }
        
                impl<ty T> Unit<> for T where [] {
                    type Assoc<> = () where [];
                }
            },
            crate foo {
                struct FooStruct<> where [] {}
                impl<> CoreTrait<> for <FooStruct<> as Unit<>>::Assoc<> where [] {}
            }
        ]",
    ));
}

#[test]
fn test_orphan_uncovered_T() {
    expect_test::expect![[r#"
        Err(
            Error {
                context: "orphan_check( impl <ty> CoreTrait < (rigid (adt FooStruct)) > for ^ty0_0 where [] { })",
                source: "failed to prove {@ IsLocal(CoreTrait(!ty_1, (rigid (adt FooStruct))))} given {}, got {}",
            },
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                trait CoreTrait<ty T> where [] {}
            },
            crate foo {
                struct FooStruct<> where [] {}
                impl<ty T> CoreTrait<FooStruct<>> for T where [] {}
            }
        ]",
    ));
}

#[test]
fn test_orphan_covered_VecT() {
    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                trait CoreTrait<ty T> where [] {}
                struct Vec<ty T> where [] {}
            },
            crate foo {
                struct FooStruct<> where [] {}
                impl<ty T> CoreTrait<FooStruct<>> for Vec<T> where [] {}
            }
        ]",
    ));
}
