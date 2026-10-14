#![allow(non_snake_case)]
use a_mir_formality::{crates, FormalityTest};

#[test]
fn neg_CoreTrait_for_CoreStruct_in_Foo() {
    FormalityTest::new(crates![crate core {
                trait CoreTrait {}
                struct CoreStruct {}
            },
            crate foo {
                #![feature(negative_impls)]
                impl !CoreTrait for CoreStruct {}
            }]).err(expect_test::expect![[r#"
                the rule "fundamental rigid type" at (is_local.rs) failed because
                  condition evaluated to false: `is_fundamental(decls, name)`
                    decls = program([crate core { trait CoreTrait <ty> { } struct CoreStruct { } }, crate foo { #![feature(negative_impls)] impl ! CoreTrait for CoreStruct {} }], 222)
                    name = (adt CoreStruct)

                crates/formality-rust/src/prove/prove_normalize.rs:21:1: no applicable rules for prove_normalize { p: CoreStruct, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

                the rule "local rigid type" at (is_local.rs) failed because
                  condition evaluated to false: `decls.is_local_adt_id(a)`
                    decls = program([crate core { trait CoreTrait <ty> { } struct CoreStruct { } }, crate foo { #![feature(negative_impls)] impl ! CoreTrait for CoreStruct {} }], 222)
                    a = CoreStruct

                the rule "local trait" at (is_local.rs) failed because
                  condition evaluated to false: `decls.is_local_trait_id(&goal.trait_id)`
                    decls = program([crate core { trait CoreTrait <ty> { } struct CoreStruct { } }, crate foo { #![feature(negative_impls)] impl ! CoreTrait for CoreStruct {} }], 222)
                    &goal.trait_id = CoreTrait"#]])
}

#[test]
fn mirror_CoreStruct() {
    FormalityTest::new(crates![crate core {
                trait CoreTrait {}
                struct CoreStruct {}

                trait Mirror {
                    type Assoc : [];
                }

                impl<T> Mirror for T {
                    type Assoc = T;
                }
            },
            crate foo {
                impl CoreTrait for <CoreStruct as Mirror>::Assoc {}
            }]).err(expect_test::expect![[r#"
                the rule "fundamental rigid type" at (is_local.rs) failed because
                  condition evaluated to false: `is_fundamental(decls, name)`
                    decls = program([crate core { trait CoreTrait <ty> { } struct CoreStruct { } trait Mirror <ty> { type Assoc : [] ; } impl <ty> Mirror for ^ty0_0 { type Assoc = ^ty1_0 ; } }, crate foo { impl CoreTrait for <CoreStruct as Mirror>::Assoc { } }], 222)
                    name = (adt CoreStruct)

                crates/formality-rust/src/prove/prove_normalize.rs:21:1: no applicable rules for prove_normalize { p: CoreStruct, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

                the rule "local rigid type" at (is_local.rs) failed because
                  condition evaluated to false: `decls.is_local_adt_id(a)`
                    decls = program([crate core { trait CoreTrait <ty> { } struct CoreStruct { } trait Mirror <ty> { type Assoc : [] ; } impl <ty> Mirror for ^ty0_0 { type Assoc = ^ty1_0 ; } }, crate foo { impl CoreTrait for <CoreStruct as Mirror>::Assoc { } }], 222)
                    a = CoreStruct

                the rule "local trait" at (is_local.rs) failed because
                  condition evaluated to false: `decls.is_local_trait_id(&goal.trait_id)`
                    decls = program([crate core { trait CoreTrait <ty> { } struct CoreStruct { } trait Mirror <ty> { type Assoc : [] ; } impl <ty> Mirror for ^ty0_0 { type Assoc = ^ty1_0 ; } }, crate foo { impl CoreTrait for <CoreStruct as Mirror>::Assoc { } }], 222)
                    &goal.trait_id = CoreTrait"#]])
}

#[test]
fn mirror_FooStruct() {
    FormalityTest::new(crates![crate core {
        trait CoreTrait {}

        trait Mirror {
            type Assoc : [];
        }

        impl<T> Mirror for T {
            type Assoc = T;
        }
    },
    crate foo {
        struct FooStruct {}
        impl CoreTrait for <FooStruct as Mirror>::Assoc {}
    }])
    .skip_execute()
    .ok()
}

#[test]
fn covered_VecT() {
    FormalityTest::new(crates![crate core {
        trait CoreTrait<T> {}
        struct Vec<T> {}
    },
    crate foo {
        struct FooStruct {}
        impl<T> CoreTrait<FooStruct> for Vec<T> {}
    }])
    .skip_execute()
    .ok()
}

#[test]
fn covered_ref_VecT() {
    FormalityTest::new(crates![crate core {
        trait CoreTrait<T> {}
        struct Vec<T> { value: T }
    },
    crate foo {
        struct FooStruct {}
        impl<'a, T> CoreTrait<FooStruct> for &'a Vec<T> where T: 'a {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok()
}

#[test]
fn covered_nested_mut_ref_VecT() {
    FormalityTest::new(crates![crate core {
        trait CoreTrait<T> {}
        struct Vec<T> { value: T }
    },
    crate foo {
        struct FooStruct {}
        impl<'a, T> CoreTrait<FooStruct> for &'a &'a mut Vec<T> where T: 'a {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok()
}

#[test]
fn uncovered_mut_ref_T() {
    FormalityTest::new(crates![crate core {
        trait CoreTrait<T> {}
    },
    crate foo {
        struct FooStruct {}
        impl<'a, T> CoreTrait<FooStruct> for &'a mut T where T: 'a {}
    }])
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ IsLocal(CoreTrait(&!lt_1 mut !ty_0, FooStruct)), via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:57:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:57:1: no applicable rules for prove_normalize_via { goal: &!lt_1 mut !ty_0, via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:57:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "rigid" at (is_local.rs) failed because
          condition evaluated to false: `!is_fundamental(decls, name)`
            decls = program([crate core { trait CoreTrait <ty, ty> { } }, crate foo { struct FooStruct { } impl <lt, ty> CoreTrait <FooStruct> for &^lt0_0 mut ^ty0_1 where ^ty0_1 : ^lt0_0 { } }], 222)
            name = &(mut)

        crates/formality-rust/src/prove/prove_normalize.rs:57:1: no applicable rules for prove_normalize_via { goal: &!lt_1 mut !ty_0, via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "local trait" at (is_local.rs) failed because
          condition evaluated to false: `decls.is_local_trait_id(&goal.trait_id)`
            decls = program([crate core { trait CoreTrait <ty, ty> { } }, crate foo { struct FooStruct { } impl <lt, ty> CoreTrait <FooStruct> for &^lt0_0 mut ^ty0_1 where ^ty0_1 : ^lt0_0 { } }], 222)
            &goal.trait_id = CoreTrait"#]])
}

#[test]
fn uncovered_nested_ref_T() {
    FormalityTest::new(crates![crate core {
        trait CoreTrait<T> {}
    },
    crate foo {
        struct FooStruct {}
        impl<'a, T> CoreTrait<FooStruct> for &'a &'a T where T: 'a {}
    }])
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_via.rs:8:1: no applicable rules for prove_via { goal: @ IsLocal(CoreTrait(&!lt_1 &!lt_1 !ty_0, FooStruct)), via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:57:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:57:1: no applicable rules for prove_normalize_via { goal: &!lt_1 !ty_0, via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:57:1: no applicable rules for prove_normalize_via { goal: &!lt_1 &!lt_1 !ty_0, via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove_normalize.rs:57:1: no applicable rules for prove_normalize_via { goal: !ty_0, via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "rigid" at (is_local.rs) failed because
          condition evaluated to false: `!is_fundamental(decls, name)`
            decls = program([crate core { trait CoreTrait <ty, ty> { } }, crate foo { struct FooStruct { } impl <lt, ty> CoreTrait <FooStruct> for &^lt0_0 &^lt0_0 ^ty0_1 where ^ty0_1 : ^lt0_0 { } }], 222)
            name = &(shared)

        crates/formality-rust/src/prove/prove_normalize.rs:57:1: no applicable rules for prove_normalize_via { goal: &!lt_1 !ty_0, via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "rigid" at (is_local.rs) failed because
          condition evaluated to false: `!is_fundamental(decls, name)`
            decls = program([crate core { trait CoreTrait <ty, ty> { } }, crate foo { struct FooStruct { } impl <lt, ty> CoreTrait <FooStruct> for &^lt0_0 &^lt0_0 ^ty0_1 where ^ty0_1 : ^lt0_0 { } }], 222)
            name = &(shared)

        crates/formality-rust/src/prove/prove_normalize.rs:57:1: no applicable rules for prove_normalize_via { goal: &!lt_1 &!lt_1 !ty_0, via: !ty_0 : !lt_1, assumptions: {!ty_0 : !lt_1}, env: Env { variables: [!lt_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "local trait" at (is_local.rs) failed because
          condition evaluated to false: `decls.is_local_trait_id(&goal.trait_id)`
            decls = program([crate core { trait CoreTrait <ty, ty> { } }, crate foo { struct FooStruct { } impl <lt, ty> CoreTrait <FooStruct> for &^lt0_0 &^lt0_0 ^ty0_1 where ^ty0_1 : ^lt0_0 { } }], 222)
            &goal.trait_id = CoreTrait"#]])
}

#[test]
fn raw_pointer_is_not_fundamental() {
    FormalityTest::new(crates![crate core {
        trait CoreTrait {}
    },
    crate foo {
        struct FooStruct {}
        impl CoreTrait for *mut FooStruct {}
    }])
    .skip_execute()
    .err(expect_test::expect![[r#"
        the rule "fundamental rigid type" at (is_local.rs) failed because
          condition evaluated to false: `is_fundamental(decls, name)`
            decls = program([crate core { trait CoreTrait <ty> { } }, crate foo { struct FooStruct { } impl CoreTrait for *mut FooStruct { } }], 222)
            name = * mut

        crates/formality-rust/src/prove/prove_normalize.rs:21:1: no applicable rules for prove_normalize { p: *mut FooStruct, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "local trait" at (is_local.rs) failed because
          condition evaluated to false: `decls.is_local_trait_id(&goal.trait_id)`
            decls = program([crate core { trait CoreTrait <ty> { } }, crate foo { struct FooStruct { } impl CoreTrait for *mut FooStruct { } }], 222)
            &goal.trait_id = CoreTrait"#]])
}

#[test]
fn uncovered_T() {
    FormalityTest::new(crates![crate core {
                trait CoreTrait<T> {}
            },
            crate foo {
                struct FooStruct {}
                impl<T> CoreTrait<FooStruct> for T {}
            }]).err(expect_test::expect![[r#"
                crates/formality-rust/src/prove/prove_normalize.rs:21:1: no applicable rules for prove_normalize { p: !ty_0, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove_normalize.rs:21:1: no applicable rules for prove_normalize { p: !ty_0, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                the rule "local trait" at (is_local.rs) failed because
                  condition evaluated to false: `decls.is_local_trait_id(&goal.trait_id)`
                    decls = program([crate core { trait CoreTrait <ty, ty> { } }, crate foo { struct FooStruct { } impl <ty> CoreTrait <FooStruct> for ^ty0_0 { } }], 222)
                    &goal.trait_id = CoreTrait"#]])
}

#[test]
fn uncovered_ref_T() {
    FormalityTest::new(crates![crate core {
                trait CoreTrait<T> {}
            },
            crate foo {
                struct FooStruct {}
                impl<T> CoreTrait<FooStruct> for &'static T {}
            }]).err(expect_test::expect![[r#"
                crates/formality-rust/src/prove/prove_normalize.rs:21:1: no applicable rules for prove_normalize { p: !ty_0, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove_normalize.rs:21:1: no applicable rules for prove_normalize { p: &' static !ty_0, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                crates/formality-rust/src/prove/prove_normalize.rs:21:1: no applicable rules for prove_normalize { p: !ty_0, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                the rule "rigid" at (is_local.rs) failed because
                  condition evaluated to false: `!is_fundamental(decls, name)`
                    decls = program([crate core { trait CoreTrait <ty, ty> { } }, crate foo { struct FooStruct { } impl <ty> CoreTrait <FooStruct> for &' static ^ty0_0 { } }], 222)
                    name = &(shared)

                crates/formality-rust/src/prove/prove_normalize.rs:21:1: no applicable rules for prove_normalize { p: &' static !ty_0, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

                the rule "local trait" at (is_local.rs) failed because
                  condition evaluated to false: `decls.is_local_trait_id(&goal.trait_id)`
                    decls = program([crate core { trait CoreTrait <ty, ty> { } }, crate foo { struct FooStruct { } impl <ty> CoreTrait <FooStruct> for &' static ^ty0_0 { } }], 222)
                    &goal.trait_id = CoreTrait"#]])
}

#[test]
fn ref_FooStruct_is_local() {
    FormalityTest::new(crates![crate core {
        trait CoreTrait {}
    },
    crate foo {
        struct FooStruct {}
        impl CoreTrait for &'static FooStruct {}
        impl CoreTrait for &'static mut FooStruct {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok()
}

#[test]
fn nested_ref_FooStruct_is_local() {
    FormalityTest::new(crates![crate core {
        trait CoreTrait {}
    },
    crate foo {
        struct FooStruct {}
        impl<'a, 'b> CoreTrait for &'a &'b mut FooStruct where 'b: 'a {}
        fn main() -> () {}
    }])
    .rustc_ok()
    .ok()
}

#[test]
fn ref_foreign_wrapper_of_local_is_not_local() {
    FormalityTest::new(crates![crate core {
        trait CoreTrait {}
        struct ForeignWrapper<T> { value: T }
    },
    crate foo {
        struct FooStruct {}
        impl<'a> CoreTrait for &'a ForeignWrapper<FooStruct> {}
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0117]: only traits defined in the current crate can be implemented for arbitrary types
         --> lib.rs
          |
        5 | impl<'a00> CoreTrait for &'a00 ForeignWrapper<FooStruct> {}
          | ^^^^^^^^^^^^^^^^^^^^^^^^^-------------------------------
          |                          |
          |                          `mycore::ForeignWrapper` is not defined in the current crate
          |
          = note: impl doesn't have any local type before any uncovered type parameters
          = note: for more information see https://doc.rust-lang.org/reference/items/implementations.html#orphan-rules
          = note: define and implement a trait or new type instead

        For more information about this error, try `rustc --explain E0117`.
        error: could not compile `foo` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        the rule "fundamental rigid type" at (is_local.rs) failed because
          condition evaluated to false: `is_fundamental(decls, name)`
            decls = program([crate core { trait CoreTrait <ty> { } struct ForeignWrapper <ty> { value : ^ty0_0 } }, crate foo { struct FooStruct { } impl <lt> CoreTrait for &^lt0_0 ForeignWrapper<FooStruct> { } }], 222)
            name = (adt ForeignWrapper)

        crates/formality-rust/src/prove/prove_normalize.rs:21:1: no applicable rules for prove_normalize { p: ForeignWrapper<FooStruct>, assumptions: {}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "local rigid type" at (is_local.rs) failed because
          condition evaluated to false: `decls.is_local_adt_id(a)`
            decls = program([crate core { trait CoreTrait <ty> { } struct ForeignWrapper <ty> { value : ^ty0_0 } }, crate foo { struct FooStruct { } impl <lt> CoreTrait for &^lt0_0 ForeignWrapper<FooStruct> { } }], 222)
            a = ForeignWrapper

        crates/formality-rust/src/prove/prove_normalize.rs:21:1: no applicable rules for prove_normalize { p: &!lt_0 ForeignWrapper<FooStruct>, assumptions: {}, env: Env { variables: [!lt_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "local trait" at (is_local.rs) failed because
          condition evaluated to false: `decls.is_local_trait_id(&goal.trait_id)`
            decls = program([crate core { trait CoreTrait <ty> { } struct ForeignWrapper <ty> { value : ^ty0_0 } }, crate foo { struct FooStruct { } impl <lt> CoreTrait for &^lt0_0 ForeignWrapper<FooStruct> { } }], 222)
            &goal.trait_id = CoreTrait"#]])
}

#[test]
fn ref_CoreStruct_is_not_local() {
    FormalityTest::new(crates![crate core {
        trait CoreTrait {}
        struct CoreStruct {}
    },
    crate foo {
        impl CoreTrait for &'static CoreStruct {}
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0117]: only traits defined in the current crate can be implemented for arbitrary types
         --> lib.rs
          |
        3 | impl CoreTrait for &'static CoreStruct {}
          | ^^^^^^^^^^^^^^^^^^^-------------------
          |                    |
          |                    `mycore::CoreStruct` is not defined in the current crate
          |
          = note: impl doesn't have any local type before any uncovered type parameters
          = note: for more information see https://doc.rust-lang.org/reference/items/implementations.html#orphan-rules
          = note: define and implement a trait or new type instead

        For more information about this error, try `rustc --explain E0117`.
        error: could not compile `foo` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        the rule "fundamental rigid type" at (is_local.rs) failed because
          condition evaluated to false: `is_fundamental(decls, name)`
            decls = program([crate core { trait CoreTrait <ty> { } struct CoreStruct { } }, crate foo { impl CoreTrait for &' static CoreStruct { } }], 222)
            name = (adt CoreStruct)

        crates/formality-rust/src/prove/prove_normalize.rs:21:1: no applicable rules for prove_normalize { p: CoreStruct, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "local rigid type" at (is_local.rs) failed because
          condition evaluated to false: `decls.is_local_adt_id(a)`
            decls = program([crate core { trait CoreTrait <ty> { } struct CoreStruct { } }, crate foo { impl CoreTrait for &' static CoreStruct { } }], 222)
            a = CoreStruct

        crates/formality-rust/src/prove/prove_normalize.rs:21:1: no applicable rules for prove_normalize { p: &' static CoreStruct, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "local trait" at (is_local.rs) failed because
          condition evaluated to false: `decls.is_local_trait_id(&goal.trait_id)`
            decls = program([crate core { trait CoreTrait <ty> { } struct CoreStruct { } }, crate foo { impl CoreTrait for &' static CoreStruct { } }], 222)
            &goal.trait_id = CoreTrait"#]])
}

#[test]
fn alias_to_unit() {
    FormalityTest::new(crates![crate core {
                trait CoreTrait {}

                trait Unit {
                    type Assoc : [];
                }

                impl<T> Unit for T {
                    type Assoc = ();
                }
            },
            crate foo {
                struct FooStruct {}
                impl CoreTrait for <FooStruct as Unit>::Assoc {}
            }]).err(expect_test::expect![[r#"
                the rule "fundamental rigid type" at (is_local.rs) failed because
                  condition evaluated to false: `is_fundamental(decls, name)`
                    decls = program([crate core { trait CoreTrait <ty> { } trait Unit <ty> { type Assoc : [] ; } impl <ty> Unit for ^ty0_0 { type Assoc = () ; } }, crate foo { struct FooStruct { } impl CoreTrait for <FooStruct as Unit>::Assoc { } }], 222)
                    name = tuple(0)

                crates/formality-rust/src/prove/prove_normalize.rs:21:1: no applicable rules for prove_normalize { p: (), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

                the rule "local trait" at (is_local.rs) failed because
                  condition evaluated to false: `decls.is_local_trait_id(&goal.trait_id)`
                    decls = program([crate core { trait CoreTrait <ty> { } trait Unit <ty> { type Assoc : [] ; } impl <ty> Unit for ^ty0_0 { type Assoc = () ; } }, crate foo { struct FooStruct { } impl CoreTrait for <FooStruct as Unit>::Assoc { } }], 222)
                    &goal.trait_id = CoreTrait"#]])
}

#[test]
fn CoreTrait_for_CoreStruct_in_Foo() {
    FormalityTest::new(crates![crate core {
                trait CoreTrait {}
                struct CoreStruct {}
            },
            crate foo {
                impl CoreTrait for CoreStruct {}
            }]).err(expect_test::expect![[r#"
                the rule "fundamental rigid type" at (is_local.rs) failed because
                  condition evaluated to false: `is_fundamental(decls, name)`
                    decls = program([crate core { trait CoreTrait <ty> { } struct CoreStruct { } }, crate foo { impl CoreTrait for CoreStruct { } }], 222)
                    name = (adt CoreStruct)

                crates/formality-rust/src/prove/prove_normalize.rs:21:1: no applicable rules for prove_normalize { p: CoreStruct, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

                the rule "local rigid type" at (is_local.rs) failed because
                  condition evaluated to false: `decls.is_local_adt_id(a)`
                    decls = program([crate core { trait CoreTrait <ty> { } struct CoreStruct { } }, crate foo { impl CoreTrait for CoreStruct { } }], 222)
                    a = CoreStruct

                the rule "local trait" at (is_local.rs) failed because
                  condition evaluated to false: `decls.is_local_trait_id(&goal.trait_id)`
                    decls = program([crate core { trait CoreTrait <ty> { } struct CoreStruct { } }, crate foo { impl CoreTrait for CoreStruct { } }], 222)
                    &goal.trait_id = CoreTrait"#]])
}

#[test]
fn CoreTraitLocal_for_AliasToKnown_in_Foo() {
    // TODO: see comment in `orphan_check` from prev commit
    FormalityTest::new(crates![crate core {
        trait CoreTrait<T> {}

        trait Unit {
            type Assoc : [];
        }

        impl<T> Unit for T {
            type Assoc = ();
        }
    },
    crate foo {
        struct FooStruct {}
        impl CoreTrait<FooStruct> for <() as Unit>::Assoc {}
    }])
    .skip_execute()
    .ok()
}
