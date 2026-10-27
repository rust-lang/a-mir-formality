//! Non-empty tuple const values need `adt_const_params` and `unsized_const_params`.

use a_mir_formality::{crates, FormalityTest};
use formality_core::test;

#[test]
fn tuple_const_without_gates_fails() {
    FormalityTest::new(crates![crate Foo {
        trait Foo<const C> where type_of_const C is (u32, bool) {}
        impl Foo<tuple(2) <> { u32(1), true }> for u32 {}
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "check crate" at (mod.rs) failed because
          non-empty tuple consts require #![feature(adt_const_params)] and #![feature(unsized_const_params)]"#]]);
}

#[test]
fn tuple_const_with_one_gate_fails() {
    FormalityTest::new(crates![crate Foo {
        #![feature(adt_const_params)]
        trait Foo<const C> where type_of_const C is (u32, bool) {}
        impl Foo<tuple(2) <> { u32(1), true }> for u32 {}
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "check crate" at (mod.rs) failed because
          non-empty tuple consts require #![feature(adt_const_params)] and #![feature(unsized_const_params)]"#]]);
}

#[test]
fn empty_tuple_const_needs_adt_const_params_only() {
    FormalityTest::new(crates![crate Foo {
        #![feature(adt_const_params)]
        trait Foo<const C> where type_of_const C is () {}
        impl Foo<tuple(0) <> { }> for u32 {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok()
}

#[test]
fn tuple_const_with_gates_compiles_with_rustc() {
    FormalityTest::new(crates![crate Foo {
        #![feature(adt_const_params)]
        #![feature(unsized_const_params)]
        trait Foo<const C> where type_of_const C is (u32, bool) {}
        impl Foo<tuple(2) <> { u32(1), true }> for u32 {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok()
}

#[test]
fn empty_tuple_const_without_gates_fails() {
    FormalityTest::new(crates![crate Foo {
        trait Foo<const C> where type_of_const C is () {}
        impl Foo<tuple(0) <> { }> for u32 {}
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "check crate" at (mod.rs) failed because
          tuple consts require #![feature(adt_const_params)]"#]]);
}
