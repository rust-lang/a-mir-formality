#![allow(non_snake_case)]

#[test]
fn nonsense_rigid_const_bound() {
    crate::assert_ok!(
        [
            crate Foo {
                // This where-clause is not *provable*, but it is well-formed.
                trait Foo where type_of_const true is u32 {}
            }
        ]
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

                impl Foo<const u32(42)> for u32 {}
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

                // Here, the impl is assuming C is u32, which mismatches the trait bound.
                impl<const C> Foo<const C> for u32 where type_of_const C is u32 {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    )
}

#[test]
fn generic_match() {
    crate::assert_ok!(
        [
            crate Foo {
                trait Foo<const C> where type_of_const C is bool {}

                // Here, the impl matches the trait bound.
                impl<const C> Foo<const C> for u32 where type_of_const C is bool {}
            }
        ]
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
