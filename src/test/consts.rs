#![allow(non_snake_case)]

#[test]
fn nonsense_rigid_const_bound() {
    crate::assert_err!(
        // This test is the short version of `generic_mismatch`, skipping
        // substituting and directly going to a wrong constant.
        [
            crate Foo {
                trait Foo where type_of_const true is u32 {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            the rule "symmetric" at (prove_eq.rs) failed because
              cyclic proof attempt: `prove_eq { a: u32, b: bool, assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }`"#]]
    )
}

#[test]
fn ok() {
    crate::assert_ok!(

        [
            crate Foo {
                trait Foo<const C> where type_of_const C is bool {}
                trait Bar<const C> where type_of_const C is u32 {}

                impl<const C> Foo<const C> for u32 where type_of_const C is bool {}
            }
        ]
    )
}

#[test]
fn mismatch() {
    crate::assert_err!(
        [
            crate Foo {
                trait Foo<const C> where type_of_const C is bool {}

                impl Foo<const 42_u32> for u32 {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    )
}

#[test]
fn holds() {
    crate::assert_ok!(

        [
            crate Foo {
                trait Foo<const C> where type_of_const C is bool {}

                impl Foo<const true> for u32 {}
            }
        ]
    )
}

#[test]
fn rigid_const_bound() {
    crate::assert_ok!(



        [
            crate Foo {
                trait Foo where type_of_const true is bool {}
            }
        ]
    )
}

#[test]
fn generic_mismatch() {
    crate::assert_err!(
        [
            crate Foo {
                trait Foo<const C> where type_of_const C is bool {}

                impl<const C> Foo<const C> for u32 where type_of_const C is u32 {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            the rule "const has ty" at (prove_wc.rs) failed because
              pattern `Some((_, const_ty))` did not match value `None`

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    )
}

#[test]
fn multiple_type_of_const() {
    crate::assert_ok!(





        [
            crate Foo {
                trait Foo<const C> where type_of_const C is bool, type_of_const C is u32 {}
            }
        ]
    )
}
