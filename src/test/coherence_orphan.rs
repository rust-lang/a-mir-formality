#![allow(non_snake_case)]

#[test]
fn neg_CoreTrait_for_CoreStruct_in_Foo() {
    crate::assert_err!(
        [
            crate core {
                trait CoreTrait {}
                struct CoreStruct {}
            },
            crate foo {
                impl !CoreTrait for CoreStruct {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            the rule "fundamental rigid type" at (is_local.rs) failed because
              condition evaluted to false: `is_fundamental(&decls, &name)`
                &decls = decls([crate core { trait CoreTrait <ty> { } struct CoreStruct { } }, crate foo { impl ! CoreTrait for CoreStruct {} }], 222)
                &name = (adt CoreStruct)

            the rule "local rigid type" at (is_local.rs) failed because
              condition evaluted to false: `decls.is_local_adt_id(&a)`
                decls = decls([crate core { trait CoreTrait <ty> { } struct CoreStruct { } }, crate foo { impl ! CoreTrait for CoreStruct {} }], 222)
                &a = CoreStruct

            the rule "local trait" at (is_local.rs) failed because
              condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                decls = decls([crate core { trait CoreTrait <ty> { } struct CoreStruct { } }, crate foo { impl ! CoreTrait for CoreStruct {} }], 222)
                &goal.trait_id = CoreTrait"#]]
    )
}

#[test]
fn mirror_CoreStruct() {
    crate::assert_err!(
        [
            crate core {
                trait CoreTrait {}
                struct CoreStruct {}

                trait Mirror {
                    type Assoc : [];
                }

                impl<ty T> Mirror for T {
                    type Assoc = T;
                }
            },
            crate foo {
                impl CoreTrait for <CoreStruct as Mirror>::Assoc {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            the rule "fundamental rigid type" at (is_local.rs) failed because
              condition evaluted to false: `is_fundamental(&decls, &name)`
                &decls = decls([crate core { trait CoreTrait <ty> { } struct CoreStruct { } trait Mirror <ty> { type Assoc : [] ; } impl <ty> Mirror for ^ty0_0 { type Assoc = ^ty1_0 ; } }, crate foo { impl CoreTrait for <CoreStruct as Mirror>::Assoc { } }], 222)
                &name = (adt CoreStruct)

            the rule "local rigid type" at (is_local.rs) failed because
              condition evaluted to false: `decls.is_local_adt_id(&a)`
                decls = decls([crate core { trait CoreTrait <ty> { } struct CoreStruct { } trait Mirror <ty> { type Assoc : [] ; } impl <ty> Mirror for ^ty0_0 { type Assoc = ^ty1_0 ; } }, crate foo { impl CoreTrait for <CoreStruct as Mirror>::Assoc { } }], 222)
                &a = CoreStruct

            the rule "local trait" at (is_local.rs) failed because
              condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                decls = decls([crate core { trait CoreTrait <ty> { } struct CoreStruct { } trait Mirror <ty> { type Assoc : [] ; } impl <ty> Mirror for ^ty0_0 { type Assoc = ^ty1_0 ; } }, crate foo { impl CoreTrait for <CoreStruct as Mirror>::Assoc { } }], 222)
                &goal.trait_id = CoreTrait"#]]
    )
}

#[test]
fn mirror_FooStruct() {
    crate::assert_ok!(
        [
            crate core {
                trait CoreTrait {}

                trait Mirror {
                    type Assoc : [];
                }

                impl<ty T> Mirror for T {
                    type Assoc = T;
                }
            },
            crate foo {
                struct FooStruct {}
                impl CoreTrait for <FooStruct as Mirror>::Assoc {}
            }
        ]
    )
}

#[test]
fn covered_VecT() {
    crate::assert_ok!(
        [
            crate core {
                trait CoreTrait<ty T> {}
                struct Vec<ty T> {}
            },
            crate foo {
                struct FooStruct {}
                impl<ty T> CoreTrait<FooStruct> for Vec<T> {}
            }
        ]
    )
}

#[test]
fn uncovered_T() {
    crate::assert_err!(
        [
            crate core {
                trait CoreTrait<ty T> {}
            },
            crate foo {
                struct FooStruct {}
                impl<ty T> CoreTrait<FooStruct> for T {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            the rule "local trait" at (is_local.rs) failed because
              condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                decls = decls([crate core { trait CoreTrait <ty, ty> { } }, crate foo { struct FooStruct { } impl <ty> CoreTrait <FooStruct> for ^ty0_0 { } }], 222)
                &goal.trait_id = CoreTrait"#]]
    )
}

#[test]
fn alias_to_unit() {
    crate::assert_err!(
        [
            crate core {
                trait CoreTrait {}

                trait Unit {
                    type Assoc : [];
                }

                impl<ty T> Unit for T {
                    type Assoc = ();
                }
            },
            crate foo {
                struct FooStruct {}
                impl CoreTrait for <FooStruct as Unit>::Assoc {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            the rule "fundamental rigid type" at (is_local.rs) failed because
              condition evaluted to false: `is_fundamental(&decls, &name)`
                &decls = decls([crate core { trait CoreTrait <ty> { } trait Unit <ty> { type Assoc : [] ; } impl <ty> Unit for ^ty0_0 { type Assoc = () ; } }, crate foo { struct FooStruct { } impl CoreTrait for <FooStruct as Unit>::Assoc { } }], 222)
                &name = tuple(0)

            the rule "local trait" at (is_local.rs) failed because
              condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                decls = decls([crate core { trait CoreTrait <ty> { } trait Unit <ty> { type Assoc : [] ; } impl <ty> Unit for ^ty0_0 { type Assoc = () ; } }, crate foo { struct FooStruct { } impl CoreTrait for <FooStruct as Unit>::Assoc { } }], 222)
                &goal.trait_id = CoreTrait"#]]
    )
}

#[test]
fn CoreTrait_for_CoreStruct_in_Foo() {
    crate::assert_err!(
        [
            crate core {
                trait CoreTrait {}
                struct CoreStruct {}
            },
            crate foo {
                impl CoreTrait for CoreStruct {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            the rule "fundamental rigid type" at (is_local.rs) failed because
              condition evaluted to false: `is_fundamental(&decls, &name)`
                &decls = decls([crate core { trait CoreTrait <ty> { } struct CoreStruct { } }, crate foo { impl CoreTrait for CoreStruct { } }], 222)
                &name = (adt CoreStruct)

            the rule "local rigid type" at (is_local.rs) failed because
              condition evaluted to false: `decls.is_local_adt_id(&a)`
                decls = decls([crate core { trait CoreTrait <ty> { } struct CoreStruct { } }, crate foo { impl CoreTrait for CoreStruct { } }], 222)
                &a = CoreStruct

            the rule "local trait" at (is_local.rs) failed because
              condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                decls = decls([crate core { trait CoreTrait <ty> { } struct CoreStruct { } }, crate foo { impl CoreTrait for CoreStruct { } }], 222)
                &goal.trait_id = CoreTrait"#]]
    )
}

#[test]
fn CoreTraitLocal_for_AliasToKnown_in_Foo() {
    // TODO: see comment in `orphan_check` from prev commit
    crate::assert_ok!(
    [
        crate core {
            trait CoreTrait<ty T> {}

            trait Unit {
                type Assoc : [];
            }

            impl<ty T> Unit for T {
                type Assoc = ();
            }
        },
        crate foo {
            struct FooStruct {}
            impl CoreTrait<FooStruct> for <() as Unit>::Assoc {}
        }
    ]
    )
}
