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

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Foo(u32, u32(42)), via: HasPlace(?ty_1), assumptions: {}, env: Env { variables: [?ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Foo(u32, u32(42)), via: HasPlace(?ty_1), assumptions: {}, env: Env { variables: [?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Foo(u32, u32(42)), via: HasPlace(?ty_1), assumptions: {}, env: Env { variables: [?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Foo(u32, u32(42)), via: PlaceRead(?ty_1, ?ty_2), assumptions: {}, env: Env { variables: [?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]
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

            crates/formality-rust/src/prove/prove/prove/prove_const_has_type.rs:11:1: no applicable rules for prove_const_has_type { constant: !const_0, assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness, pending: [], allow_pending_outlives: false }, decls: program([crate core { trait HasPlace <ty> { type Target : [] ; } unsafe trait Projection <ty> { type Source : [] ; type Target : [] ; fn offset (self : ^ty1_0) -> usize ; } unsafe trait PlaceRead <ty, ty> where ^ty0_0 : HasPlace, ^ty0_1 : Projection, <^ty0_1 as Projection>::Source => <^ty0_0 as HasPlace>::Target { unsafe fn read (this : *const ^ty1_0, proj : ^ty1_1) -> <^ty1_1 as Projection>::Target ; } unsafe trait PlaceWrite <ty, ty> where ^ty0_0 : HasPlace, ^ty0_1 : Projection, <^ty0_1 as Projection>::Source => <^ty0_0 as HasPlace>::Target { unsafe fn write (this : *const ^ty1_0, proj : ^ty1_1, value : <^ty1_1 as Projection>::Target) -> () ; } unsafe trait PlaceMove <ty, ty> where ^ty0_0 : PlaceRead <^ty0_1>, ^ty0_1 : Projection, <^ty0_1 as Projection>::Source => <^ty0_0 as HasPlace>::Target { } unsafe trait DropHusk <ty> where ^ty0_0 : HasPlace { unsafe fn drop_husk (this : *const ^ty1_0) -> () ; } unsafe trait PlaceDrop <ty, ty> where ^ty0_0 : HasPlace, ^ty0_1 : Projection, <^ty0_1 as Projection>::Source => <^ty0_0 as HasPlace>::Target { unsafe fn drop (this : *const ^ty1_0, proj : ^ty1_1) -> () ; } unsafe trait PlaceBorrow <ty, ty, ty> where ^ty0_0 : HasPlace, ^ty0_1 : Projection, <^ty0_1 as Projection>::Source => <^ty0_0 as HasPlace>::Target { type BorrowDuration : [BorrowDuration] ; unsafe fn borrow (this : *const ^ty1_0, proj : ^ty1_1) -> ^ty1_2 ; } trait BorrowDuration <ty> { } struct Instant { } struct Lifetime <lt> { } struct Indefinite { } impl BorrowDuration for Instant { } impl <lt> BorrowDuration for Lifetime<^lt0_0> { } impl BorrowDuration for Indefinite { } unsafe trait PlaceWrapper <ty, ty> where ^ty0_0 : HasPlace, ^ty0_1 : Projection, <^ty0_1 as Projection>::Source => <^ty0_0 as HasPlace>::Target { type WrappedProjection : [Projection] where <<^ty1_0 as PlaceWrapper<^ty1_1>>::WrappedProjection as Projection>::Source => ^ty1_0 ; fn wrap_projection (proj : ^ty1_1) -> <^ty1_0 as PlaceWrapper<^ty1_1>>::WrappedProjection ; } unsafe trait PlaceDeref <ty, ty> where ^ty0_0 : HasPlace, ^ty0_1 : Projection, <^ty0_1 as Projection>::Source => <^ty0_0 as HasPlace>::Target, <^ty0_1 as Projection>::Target : HasPlace { unsafe fn deref (ptr : *mut ^ty1_0, proj : ^ty1_1) -> *const <^ty1_1 as Projection>::Target ; } }, crate Foo { trait Foo <ty, const> where type_of_const ^const0_1 is bool { } impl <const> Foo <^const0_0> for u32 where type_of_const ^const0_0 is u32 { } }], 222) }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Foo(u32, !const_0), via: HasPlace(?ty_1), assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0, ?ty_1, ?ty_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Foo(u32, !const_0), via: HasPlace(?ty_1), assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0, ?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Foo(u32, !const_0), via: HasPlace(?ty_1), assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Foo(u32, !const_0), via: PlaceRead(?ty_1, ?ty_2), assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0, ?ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]
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
