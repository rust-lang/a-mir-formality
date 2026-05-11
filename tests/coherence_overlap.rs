#![allow(non_snake_case)] // we embed type names into the names for our test functions

use a_mir_formality::{assert_err, assert_ok, test_program_ok};
use formality_core::test_util::ResultTestExt;
use formality_macros::test;

#[test]
fn test_overlap_normalize_alias_to_LocalType() {
    // `LocalTrait` has a blanket impl for all `T: Iterator`
    // and then an impl for `<LocalType as Mirror>::T`...

    let gen_program = |addl: &str| {
        const BASE_PROGRAM: &str = "[
            crate core {
                trait Iterator {
                }

                trait Mirror {
                    type T : [];
                }

                impl<A> Mirror for A {
                    type T = A;
                }

                struct LocalType {}

                trait LocalTrait { }

                impl<T> LocalTrait for T where T: Iterator { }

                impl LocalTrait for <LocalType as Mirror>::T { }

                ADDITIONAL
            }
        ]";

        BASE_PROGRAM.replace("ADDITIONAL", addl)
    };

    // ...on its own, this is OK. Figuring this out, though, requires proving
    // `<LocalType as Mirror>::T: Iterator` which requires normalizing
    // the alias to `LocalType`...

    test_program_ok(&gen_program("")).assert_ok();

    // ...but it's an error if LocalType implements Iterator (figuring *this* out also
    // requires normalizing).

    test_program_ok(&gen_program("impl Iterator for LocalType {}")).assert_err(
        expect_test::expect![[r#"judgment `check_all_crates { crates: [crate core { trait Iterator <ty> { } trait Mirror <ty> { type T : [] ; } impl <ty> Mirror for ^ty0_0 { type T = ^ty1_0 ; } struct LocalType { } trait LocalTrait <ty> { } impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { } impl LocalTrait for <LocalType as Mirror>::T { } impl Iterator for LocalType { } }] }` failed at the following rule(s):
  the rule "check all prefixes" at (mod.rs) failed because
    judgment `check_crate { program: program([crate core { trait Iterator <ty> { } trait Mirror <ty> { type T : [] ; } impl <ty> Mirror for ^ty0_0 { type T = ^ty1_0 ; } struct LocalType { } trait LocalTrait <ty> { } impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { } impl LocalTrait for <LocalType as Mirror>::T { } impl Iterator for LocalType { } }], 222), c: crate core { trait Iterator <ty> { } trait Mirror <ty> { type T : [] ; } impl <ty> Mirror for ^ty0_0 { type T = ^ty1_0 ; } struct LocalType { } trait LocalTrait <ty> { } impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { } impl LocalTrait for <LocalType as Mirror>::T { } impl Iterator for LocalType { } } }` failed at the following rule(s):
      the rule "check crate" at (mod.rs) failed because
        judgment `check_coherence { program: program([crate core { trait Iterator <ty> { } trait Mirror <ty> { type T : [] ; } impl <ty> Mirror for ^ty0_0 { type T = ^ty1_0 ; } struct LocalType { } trait LocalTrait <ty> { } impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { } impl LocalTrait for <LocalType as Mirror>::T { } impl Iterator for LocalType { } }], 222), current_crate: crate core { trait Iterator <ty> { } trait Mirror <ty> { type T : [] ; } impl <ty> Mirror for ^ty0_0 { type T = ^ty1_0 ; } struct LocalType { } trait LocalTrait <ty> { } impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { } impl LocalTrait for <LocalType as Mirror>::T { } impl Iterator for LocalType { } } }` failed at the following rule(s):
          the rule "check_coherence" at (coherence.rs) failed because
            impls may overlap:
            impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { }
            impl LocalTrait for <LocalType as Mirror>::T { }"#]],
    );
}

#[test]
fn test_overlap_alias_not_normalizable() {
    // `LocalTrait` has a blanket impl for all `T: Iterator`
    // and then an impl for `<T as Mirror>::T`...

    let gen_program = |addl: &str| {
        const BASE_PROGRAM: &str = "[
            crate core {
                trait Iterator {
                }

                trait Mirror {
                    type T : [];
                }

                impl<A> Mirror for A {
                    type T = A;
                }

                struct LocalType {}

                trait LocalTrait { }

                impl<T> LocalTrait for T where T: Iterator { }

                impl<T> LocalTrait for <T as Mirror>::T where T: Mirror { }

                ADDITIONAL
            }
        ]";

        BASE_PROGRAM.replace("ADDITIONAL", addl)
    };

    // ...you get an error here, because a downstream crate could implement
    // trait for some local type, in which case it would overlap.

    test_program_ok(&gen_program("")).assert_err(expect_test::expect![[r#"judgment `check_all_crates { crates: [crate core { trait Iterator <ty> { } trait Mirror <ty> { type T : [] ; } impl <ty> Mirror for ^ty0_0 { type T = ^ty1_0 ; } struct LocalType { } trait LocalTrait <ty> { } impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { } impl <ty> LocalTrait for <^ty0_0 as Mirror>::T where ^ty0_0 : Mirror { } }] }` failed at the following rule(s):
  the rule "check all prefixes" at (mod.rs) failed because
    judgment `check_crate { program: program([crate core { trait Iterator <ty> { } trait Mirror <ty> { type T : [] ; } impl <ty> Mirror for ^ty0_0 { type T = ^ty1_0 ; } struct LocalType { } trait LocalTrait <ty> { } impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { } impl <ty> LocalTrait for <^ty0_0 as Mirror>::T where ^ty0_0 : Mirror { } }], 222), c: crate core { trait Iterator <ty> { } trait Mirror <ty> { type T : [] ; } impl <ty> Mirror for ^ty0_0 { type T = ^ty1_0 ; } struct LocalType { } trait LocalTrait <ty> { } impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { } impl <ty> LocalTrait for <^ty0_0 as Mirror>::T where ^ty0_0 : Mirror { } } }` failed at the following rule(s):
      the rule "check crate" at (mod.rs) failed because
        judgment `check_coherence { program: program([crate core { trait Iterator <ty> { } trait Mirror <ty> { type T : [] ; } impl <ty> Mirror for ^ty0_0 { type T = ^ty1_0 ; } struct LocalType { } trait LocalTrait <ty> { } impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { } impl <ty> LocalTrait for <^ty0_0 as Mirror>::T where ^ty0_0 : Mirror { } }], 222), current_crate: crate core { trait Iterator <ty> { } trait Mirror <ty> { type T : [] ; } impl <ty> Mirror for ^ty0_0 { type T = ^ty1_0 ; } struct LocalType { } trait LocalTrait <ty> { } impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { } impl <ty> LocalTrait for <^ty0_0 as Mirror>::T where ^ty0_0 : Mirror { } } }` failed at the following rule(s):
          the rule "check_coherence" at (coherence.rs) failed because
            impls may overlap:
            impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { }
            impl <ty> LocalTrait for <^ty0_0 as Mirror>::T where ^ty0_0 : Mirror { }"#]]);

    // ...and if there is at least one Iterator impl, we also flag an error.

    test_program_ok(&gen_program("impl Iterator for u32 {}")).assert_err(expect_test::expect![[r#"judgment `check_all_crates { crates: [crate core { trait Iterator <ty> { } trait Mirror <ty> { type T : [] ; } impl <ty> Mirror for ^ty0_0 { type T = ^ty1_0 ; } struct LocalType { } trait LocalTrait <ty> { } impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { } impl <ty> LocalTrait for <^ty0_0 as Mirror>::T where ^ty0_0 : Mirror { } impl Iterator for u32 { } }] }` failed at the following rule(s):
  the rule "check all prefixes" at (mod.rs) failed because
    judgment `check_crate { program: program([crate core { trait Iterator <ty> { } trait Mirror <ty> { type T : [] ; } impl <ty> Mirror for ^ty0_0 { type T = ^ty1_0 ; } struct LocalType { } trait LocalTrait <ty> { } impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { } impl <ty> LocalTrait for <^ty0_0 as Mirror>::T where ^ty0_0 : Mirror { } impl Iterator for u32 { } }], 222), c: crate core { trait Iterator <ty> { } trait Mirror <ty> { type T : [] ; } impl <ty> Mirror for ^ty0_0 { type T = ^ty1_0 ; } struct LocalType { } trait LocalTrait <ty> { } impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { } impl <ty> LocalTrait for <^ty0_0 as Mirror>::T where ^ty0_0 : Mirror { } impl Iterator for u32 { } } }` failed at the following rule(s):
      the rule "check crate" at (mod.rs) failed because
        judgment `check_coherence { program: program([crate core { trait Iterator <ty> { } trait Mirror <ty> { type T : [] ; } impl <ty> Mirror for ^ty0_0 { type T = ^ty1_0 ; } struct LocalType { } trait LocalTrait <ty> { } impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { } impl <ty> LocalTrait for <^ty0_0 as Mirror>::T where ^ty0_0 : Mirror { } impl Iterator for u32 { } }], 222), current_crate: crate core { trait Iterator <ty> { } trait Mirror <ty> { type T : [] ; } impl <ty> Mirror for ^ty0_0 { type T = ^ty1_0 ; } struct LocalType { } trait LocalTrait <ty> { } impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { } impl <ty> LocalTrait for <^ty0_0 as Mirror>::T where ^ty0_0 : Mirror { } impl Iterator for u32 { } } }` failed at the following rule(s):
          the rule "check_coherence" at (coherence.rs) failed because
            impls may overlap:
            impl <ty> LocalTrait for ^ty0_0 where ^ty0_0 : Iterator { }
            impl <ty> LocalTrait for <^ty0_0 as Mirror>::T where ^ty0_0 : Mirror { }"#]]);
}

#[test]
fn u32_not_u32_impls() {
    assert_err!(
        // Test that a positive and negative impl for the same type (`u32`, here) is rejected.
        [
            crate core {
                trait Foo {}
                impl Foo for u32 {}
                impl !Foo for u32 {}
            }
        ]

        expect_test::expect![[r#"
            failed at (proven_set.rs) because
              found an unconditionally true solution Constraints { env: Env { variables: [], bias: Completeness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }"#]]
    )
}

#[test]
fn neg_CoreTrait_for_CoreStruct_implies_no_overlap() {
    assert_ok!(



        [
            crate core {
                trait CoreTrait {}
                struct CoreStruct {}
                impl !CoreTrait for CoreStruct {}
            },
            crate foo {
                trait FooTrait {}
                impl<T> FooTrait for T where T: CoreTrait {}
                impl FooTrait for CoreStruct {}
            }
        ]
    )
}

#[test]
fn foo_crate_cannot_assume_CoreStruct_does_not_impl_CoreTrait() {
    assert_err!(
        [
            crate core {
                trait CoreTrait {}
                struct CoreStruct {}
            },
            crate foo {
                trait FooTrait {}
                impl<T> FooTrait for T where T: CoreTrait {}
                impl FooTrait for CoreStruct {}
            }
        ]

        expect_test::expect![[r#"
            the rule "check_coherence" at (coherence.rs) failed because
              impls may overlap:
              impl <ty> FooTrait for ^ty0_0 where ^ty0_0 : CoreTrait { }
              impl FooTrait for CoreStruct { }"#]]
    )
}

#[test]
fn T_where_Foo_not_u32_impls() {
    assert_err!(
        // Test positive impl that has a where-clause which checks for itself,
        // i.e., `T: Foo where T: Foo`. This `T: Foo` where-clause isn't harmful
        // in the coinductive interpretation of trait matching, it actually
        // doesn't change the meaning of the impl at all. However, this formulation
        // was erroneously accepted by an earlier variant of negative impls.
        [
            crate core {
                trait Foo {}
                impl<T> Foo for T where T: Foo {}
                impl !Foo for u32 {}
            }
        ]

        expect_test::expect![[r#"
            the rule "check_trait_impl" at (impls.rs) failed because
              failed to prove {! Foo(!ty_1)} given {Foo(!ty_1)}, got [Constraints { env: Env { variables: [!ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: false, substitution: {} }]"#]]
    )
}

#[test]
fn u32_T_where_T_Is_impls() {
    assert_err!(
        // Test that we detect "indirect" overlap -- here `Foo` is implemented for `u32`
        // and also all `T: Is`, and `u32: Is`.
        [
            crate core {
                trait Foo {}
                impl Foo for u32 {}
                impl<T> Foo for T where T: Is {}

                trait Is {}
                impl Is for u32 {}
            }
        ]

        expect_test::expect![[r#"
            the rule "check_coherence" at (coherence.rs) failed because
              impls may overlap:
              impl Foo for u32 { }
              impl <ty> Foo for ^ty0_0 where ^ty0_0 : Is { }"#]]
    )
}

#[test]
fn u32_T_where_T_Not_impls() {
    assert_ok!(






        [
            crate core {
                trait Foo {}
                impl Foo for u32 {}
                impl<T> Foo for T where T: Not {}

                trait Not {}
            }
        ]
    )
}

#[test]
fn u32_u32_impls() {
    assert_err!(
        [
            crate core {
                trait Foo {}
                impl Foo for u32 {}
                impl Foo for u32 {}
            }
        ]

        expect_test::expect![[r#"
            the rule "check_coherence" at (coherence.rs) failed because
              duplicate impl in current crate: impl Foo for u32 { }"#]]
    )
}

#[test]
fn u32_i32_impls() {
    assert_ok!(

        [
            crate core {
                trait Foo {}
                impl Foo for u32 {}
                impl Foo for i32 {}
            }
        ]
    )
}

#[test]
fn u32_T_impls() {
    assert_err!(
        [
            crate core {
                trait Foo {}
                impl Foo for u32 {}
                impl<T> Foo for T {}
            }
        ]

        expect_test::expect![[r#"
            the rule "check_coherence" at (coherence.rs) failed because
              impls may overlap:
              impl Foo for u32 { }
              impl <ty> Foo for ^ty0_0 { }"#]]
    )
}

#[test]
fn T_and_T_bar() {
    assert_err! {
        [
            crate core {
                trait Foo { }

                trait Bar { }

                impl<T> Foo for T { }

                impl<T> Foo for T where T: Bar { }
            }
        ]

        expect_test::expect![[r#"
            the rule "check_coherence" at (coherence.rs) failed because
              impls may overlap:
              impl <ty> Foo for ^ty0_0 { }
              impl <ty> Foo for ^ty0_0 where ^ty0_0 : Bar { }"#]]
    }
}

#[test]
fn T_and_Local_Bar_T() {
    assert_err! {
        [
            crate core {
                trait Foo { }

                trait Bar<U> { }

                impl<T> Foo for T { }

                impl<T> Foo for T where LocalType: Bar<T> { }

                struct LocalType { }
            }
        ]

        expect_test::expect![[r#"
            the rule "check_coherence" at (coherence.rs) failed because
              impls may overlap:
              impl <ty> Foo for ^ty0_0 { }
              impl <ty> Foo for ^ty0_0 where LocalType : Bar <^ty0_0> { }"#]]
    }
}

#[test]
fn is_local_unknowable_trait_ref() {
    assert_ok! {
        [
            crate core {
                trait Project {
                    type Assoc: [];
                }

                impl<T> Project for T {
                    type Assoc = T;
                }

                trait Foo<U> { }
            },
            crate foo {
                struct LocalType {}

                trait Overlap<U> {}
                impl<T, U> Overlap<U> for T
                where
                    <T as Project>::Assoc: Foo<U> {}
                impl<T> Overlap<LocalType> for () {}
            }
        ]
    }
}

#[test]
fn is_local_with_unconstrained_self_ty_blanket_impl() {
    // TODO: this test should pass imho
    assert_err! {
        [
            crate core {
                trait Project {
                    type Assoc: [];
                }

                impl<T> Project for T {
                    type Assoc = ();
                }

                trait Foo<U> { }
            },
            crate foo {
                struct LocalType {}
                impl Foo<LocalType> for () {}

                trait Overlap<U> {}
                impl<T, U> Overlap<U> for T
                where
                    <T as Project>::Assoc: Foo<U> {}
                impl<T> Overlap<LocalType> for T {}
            }
        ]

        expect_test::expect![[r#"
            the rule "check_coherence" at (coherence.rs) failed because
              impls may overlap:
              impl <ty, ty> Overlap <^ty0_1> for ^ty0_0 where <^ty0_0 as Project>::Assoc : Foo <^ty0_1> { }
              impl <ty> Overlap <LocalType> for ^ty0_0 { }"#]]
    }
}
