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

                impl<const C> Foo<C> for u32 where type_of_const C is bool {}
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

                impl Foo<u32(42)> for u32 {}
            }
        ]

        expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:19:1: no applicable rules for prove_normalize { p: u32, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:19:1: no applicable rules for prove_normalize { p: bool, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

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

                impl Foo<true> for u32 {}
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
                impl<const C> Foo<C> for u32 where type_of_const C is u32 {}
            }
        ]

        expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Foo(u32, !const_0), via: @ ConstHasType(!const_0 , u32), assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: u32 = bool, via: @ ConstHasType(!const_0 , u32), assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ ConstHasType(!const_0 , u32), assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ ConstHasType(!const_0 , u32), assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_const_has_type.rs:11:1: no applicable rules for prove_const_has_type { constant: !const_0, assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness, pending: [], allow_pending_outlives: false }, decls: program([crate Foo { trait Foo <ty, const> where type_of_const ^const0_1 is bool { } impl <const> Foo <^const0_0> for u32 where type_of_const ^const0_0 is u32 { } }], 222) }

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
                impl<const C> Foo<C> for u32 where type_of_const C is bool {}
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
