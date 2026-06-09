#![allow(non_snake_case)]
use a_mir_formality::{crates, FormalityTest};

#[test]
fn nonsense_rigid_const_bound() {
    FormalityTest::new(crates![crate Foo {
        // This where-clause is not *provable*, but it is well-formed.
        trait Foo where type_of_const true is u32 {}
    }])
    .skip_execute()
    .ok()
}

#[test]
fn ok() {
    FormalityTest::new(crates![crate Foo {
        trait Foo<const C> where type_of_const C is bool {}
        trait Bar<const C> where type_of_const C is u32 {}

        impl<const C> Foo<C> for u32 where type_of_const C is bool {}
    }])
    .skip_execute()
    .ok()
}

#[test]
fn mismatch() {
    FormalityTest::new(crates![crate Foo {
                trait Foo<const C> where type_of_const C is bool {}

                impl Foo<u32(42)> for u32 {}
            }]).err(expect_test::expect![[r#"
                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:19:1: no applicable rules for prove_normalize { p: u32, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:19:1: no applicable rules for prove_normalize { p: bool, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

                the rule "trait implied bound" at (prove_wc.rs) failed because
                  expression evaluated to an empty collection: `decls.trait_invariants()`"#]])
}

#[test]
fn holds() {
    FormalityTest::new(crates![crate Foo {
        trait Foo<const C> where type_of_const C is bool {}

        impl Foo<true> for u32 {}
    }])
    .skip_execute()
    .ok()
}

#[test]
fn rigid_const_bound() {
    FormalityTest::new(crates![crate Foo {
        trait Foo where type_of_const true is bool {}
    }])
    .skip_execute()
    .ok()
}

#[test]
fn generic_mismatch() {
    FormalityTest::new(crates![crate Foo {
                trait Foo<const C> where type_of_const C is bool {}

                // Here, the impl is assuming C is u32, which mismatches the trait bound.
                impl<const C> Foo<C> for u32 where type_of_const C is u32 {}
            }]).err(expect_test::expect![[r#"
                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Foo(u32, !const_0), via: @ ConstHasType(!const_0 , u32), assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: u32 = bool, via: @ ConstHasType(!const_0 , u32), assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: @ ConstHasType(!const_0 , u32), assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: bool, via: @ ConstHasType(!const_0 , u32), assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove/prove/prove_const_has_type.rs:11:1: no applicable rules for prove_const_has_type { constant: !const_0, assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness, pending: [], allow_pending_outlives: false }, decls: program([crate core { trait Copy <ty> { } impl Copy for () { } impl Copy for u8 { } impl Copy for u16 { } impl Copy for u32 { } impl Copy for u64 { } impl Copy for i8 { } impl Copy for i16 { } impl Copy for i32 { } impl Copy for i64 { } impl Copy for bool { } impl Copy for usize { } impl Copy for isize { } impl <lt, ty> Copy for &^lt0_0 ^ty0_1 { } trait Derefable <ty> { type Target : [] ; } impl <lt, ty> Derefable for &^lt0_0 ^ty0_1 where ^ty0_1 : ^lt0_0 { type Target = ^ty1_1 ; } impl <lt, ty> Derefable for &mut ^lt0_0 ^ty0_1 where ^ty0_1 : ^lt0_0 { type Target = ^ty1_1 ; } }, crate Foo { trait Foo <ty, const> where type_of_const ^const0_1 is bool { } impl <const> Foo <^const0_0> for u32 where type_of_const ^const0_0 is u32 { } }], 222) }

                the rule "trait implied bound" at (prove_wc.rs) failed because
                  expression evaluated to an empty collection: `decls.trait_invariants()`"#]])
}

#[test]
fn generic_match() {
    FormalityTest::new(crates![crate Foo {
        trait Foo<const C> where type_of_const C is bool {}

        // Here, the impl matches the trait bound.
        impl<const C> Foo<C> for u32 where type_of_const C is bool {}
    }])
    .skip_execute()
    .ok()
}

#[test]
fn multiple_type_of_const() {
    FormalityTest::new(crates![crate Foo {
        trait Foo<const C> where type_of_const C is bool, type_of_const C is u32 {}
    }])
    .skip_execute()
    .ok()
}
