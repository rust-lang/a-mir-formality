#![allow(non_snake_case)] // we embed type names into the names for our test functions

use formality::test_program_ok;
use formality_macros::test;

#[test]
fn test_u32_i32_impls() {
    // Test that we permit impls for distinct types.
    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                trait Foo<> where [] {}
                impl<> Foo<> for u32 where [] {}
                impl<> Foo<> for i32 where [] {}
            }
        ]",
    ));
}

#[test]
fn test_u32_u32_impls() {
    // Test that we detect duplicate impls.
    expect_test::expect![[r#"
        Err(
            "duplicate impl in current crate: Safe impl <> Foo < > for (rigid (scalar u32)) where [] { }",
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                trait Foo<> where [] {}
                impl<> Foo<> for u32 where [] {}
                impl<> Foo<> for u32 where [] {}
            }
        ]",
    ));
}

#[test]
fn test_u32_T_impls() {
    // Test that we detect overlap involving generic parameters.
    expect_test::expect![[r#"
        Err(
            "impls may overlap: `Safe impl <> Foo < > for (rigid (scalar u32)) where [] { }` vs `Safe impl <ty> Foo < > for ^ty0_0 where [] { }`",
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                trait Foo<> where [] {}
                impl<> Foo<> for u32 where [] {}
                impl<ty T> Foo<> for T where [] {}
            }
        ]",
    ));
}

#[test]
fn test_u32_T_where_T_Not_impls() {
    // Test that, within a crate, we are able to rely on the fact
    // that `u32: Not` is not implemented.
    //
    // See also test_foo_crate_cannot_assume_CoreStruct_does_not_impl_CoreTrait
    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                trait Foo<> where [] {}
                impl<> Foo<> for u32 where [] {}
                impl<ty T> Foo<> for T where [T: Not<>] {}

                trait Not<> where [] {}
            }
        ]",
    ));
}

#[test]
fn test_u32_T_where_T_Is_impls() {
    // Test that we detect "indirect" overlap -- here `Foo` is implemented for `u32`
    // and also all `T: Is`, and `u32: Is`.
    expect_test::expect![[r#"
        Err(
            "impls may overlap: `Safe impl <> Foo < > for (rigid (scalar u32)) where [] { }` vs `Safe impl <ty> Foo < > for ^ty0_0 where [^ty0_0 : Is < >] { }`",
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                trait Foo<> where [] {}
                impl<> Foo<> for u32 where [] {}
                impl<ty T> Foo<> for T where [T: Is<>] {}

                trait Is<> where [] {}
                impl<> Is<> for u32 where [] {}
            }
        ]",
    ));
}

#[test]
fn test_u32_not_u32_impls() {
    // Test that a positive and negative impl for the same type (`u32`, here) is rejected.
    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait_impl(Safe impl <> Foo < > for (rigid (scalar u32)) where [] { })",
                source: "failed to disprove {! Foo((rigid (scalar u32)))} given {}, got {Constraints { env: Env { variables: [], coherence_mode: false }, known_true: true, substitution: {} }}",
            },
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                trait Foo<> where [] {}
                impl<> Foo<> for u32 where [] {}
                impl<> !Foo<> for u32 where [] {}
            }
        ]",
    ));
}

#[test]
fn test_T_where_Foo_not_u32_impls() {
    // Test positive impl that has a where-clause which checks for itself,
    // i.e., `T: Foo where T: Foo`. This `T: Foo` where-clause isn't harmful
    // in the coinductive interpretation of trait matching, it actually
    // doesn't change the meaning of the impl at all. However, this formulation
    // was erroneously accepted by an earlier variant of negative impls.
    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait_impl(Safe impl <ty> Foo < > for ^ty0_0 where [^ty0_0 : Foo < >] { })",
                source: "failed to disprove {! Foo(!ty_1)} given {Foo(!ty_1)}, got {Constraints { env: Env { variables: [?ty_1], coherence_mode: false }, known_true: true, substitution: {?ty_1 => (rigid (scalar u32))} }}",
            },
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                trait Foo<> where [] {}
                impl<ty T> Foo<> for T where [T: Foo<>] {}
                impl<> !Foo<> for u32 where [] {}
            }
        ]",
    ));
}

#[test]
fn test_foo_crate_cannot_assume_CoreStruct_does_not_impl_CoreTrait() {
    expect_test::expect![[r#"
        Err(
            "impls may overlap: `Safe impl <ty> FooTrait < > for ^ty0_0 where [^ty0_0 : CoreTrait < >] { }` vs `Safe impl <> FooTrait < > for (rigid (adt CoreStruct)) where [] { }`",
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                trait CoreTrait<> where [] {}
                struct CoreStruct<> where [] {}
            },
            crate foo {
                trait FooTrait<> where [] {}
                impl<ty T> FooTrait<> for T where [T: CoreTrait<>] {}
                impl<> FooTrait<> for CoreStruct<> where [] {}
            }
        ]",
    ));
}

#[test]
fn test_neg_CoreTrait_for_CoreStruct_implies_no_overlap() {
    // Variant of test_foo_crate_cannot_assume_CoreStruct_does_not_impl_CoreTrait
    // where there is a negative impl, so it is accepted.

    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(
        "[
            crate core {
                trait CoreTrait<> where [] {}
                struct CoreStruct<> where [] {}
                impl<> !CoreTrait<> for CoreStruct<> where [] {}
            },
            crate foo {
                trait FooTrait<> where [] {}
                impl<ty T> FooTrait<> for T where [T: CoreTrait<>] {}
                impl<> FooTrait<> for CoreStruct<> where [] {}
            }
        ]",
    ));
}

#[test]
fn test_overlap_normalize_alias_to_LocalType() {
    // `LocalTrait` has a blanket impl for all `T: Iterator`
    // and then an impl for `<LocalType as Mirror>::T`...

    let gen_program = |addl: &str| {
        const BASE_PROGRAM: &str = "[
            crate core {
                trait Iterator<> where [] {
                }

                trait Mirror<> where [] {
                    type T<> : [] where [];
                }
                
                impl<ty A> Mirror<> for A where [] {
                    type T<> = A where [];
                }
                
                struct LocalType<> where [] {}
                
                trait LocalTrait<> where [] { }
                
                impl<ty T> LocalTrait<> for T where [T: Iterator<>] { }
                
                impl<> LocalTrait<> for <LocalType as Mirror>::T where [] { }

                ADDITIONAL
            }
        ]";

        BASE_PROGRAM.replace("ADDITIONAL", addl)
    };

    // ...on its own, this is OK. Figuring this out, though, requires proving
    // `<LocalType as Mirror>::T: Iterator` which requires normalizing
    // the alias to `LocalType`...

    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(&gen_program("")));

    // ...but it's an error if LocalType implements Iterator (figuring *this* out also
    // requires normalizing).

    expect_test::expect![[r#"
        Err(
            "impls may overlap: `Safe impl <ty> LocalTrait < > for ^ty0_0 where [^ty0_0 : Iterator < >] { }` vs `Safe impl <> LocalTrait < > for (alias (Mirror :: T) (rigid (adt LocalType))) where [] { }`",
        )
    "#]]
    .assert_debug_eq(&test_program_ok(&gen_program("impl<> Iterator<> for LocalType<> where [] {}")));
}

#[test]
fn test_overlap_alias_not_normalizable() {
    // `LocalTrait` has a blanket impl for all `T: Iterator`
    // and then an impl for `<T as Mirror>::T`...

    let gen_program = |addl: &str| {
        const BASE_PROGRAM: &str = "[
            crate core {
                trait Iterator<> where [] {
                }

                trait Mirror<> where [] {
                    type T<> : [] where [];
                }
                
                impl<ty A> Mirror<> for A where [] {
                    type T<> = A where [];
                }
                
                struct LocalType<> where [] {}
                
                trait LocalTrait<> where [] { }
                
                impl<ty T> LocalTrait<> for T where [T: Iterator<>] { }
                
                impl<ty T> LocalTrait<> for <T as Mirror>::T where [T: Mirror<>] { }

                ADDITIONAL
            }
        ]";

        BASE_PROGRAM.replace("ADDITIONAL", addl)
    };

    // ...you might expect an error here, because we have an impl for all `T` and another
    // impl for all `T: Iterator`, but we don't flag it as one because
    // Iterator is a local trait and we can see that nobody has implemented it...
    //
    // FIXME: rustc DOES flag an error here. I think this is because the trait solver
    // refuses to solve `?X: Iterator`; we haven't implemented that rule and I haven't
    // decided how to think about it.

    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&test_program_ok(&gen_program("")));

    // ...as long as there is at least one Iterator impl, however, we do flag an error.

    expect_test::expect![[r#"
        Err(
            "impls may overlap: `Safe impl <ty> LocalTrait < > for ^ty0_0 where [^ty0_0 : Iterator < >] { }` vs `Safe impl <ty> LocalTrait < > for (alias (Mirror :: T) ^ty0_0) where [^ty0_0 : Mirror < >] { }`",
        )
    "#]] // FIXME
    .assert_debug_eq(&test_program_ok(&gen_program(
        "impl<> Iterator<> for u32 where[] {}",
    )));
}
