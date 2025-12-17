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
                &decls = decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {}, {}, {})
                &name = (adt CoreStruct)

            the rule "local rigid type" at (is_local.rs) failed because
              condition evaluted to false: `decls.is_local_adt_id(&a)`
                decls = decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {}, {}, {})
                &a = CoreStruct

            the rule "local trait" at (is_local.rs) failed because
              condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                decls = decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct { struct { } }], {}, {}, {})
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
                &decls = decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<CoreStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt CoreStruct { struct { } }], {}, {}, {})
                &name = (adt CoreStruct)

            the rule "local rigid type" at (is_local.rs) failed because
              condition evaluted to false: `decls.is_local_adt_id(&a)`
                decls = decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<CoreStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt CoreStruct { struct { } }], {}, {}, {})
                &a = CoreStruct

            the rule "local trait" at (is_local.rs) failed because
              condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                decls = decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<CoreStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt CoreStruct { struct { } }], {}, {}, {})
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
                decls = decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(^ty0_0, FooStruct)], [], [], [], [adt FooStruct { struct { } }], {}, {FooStruct}, {})
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
                &decls = decls(222, [trait CoreTrait <ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<FooStruct as Unit>::Assoc)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct}, {})
                &name = tuple(0)

            the rule "local trait" at (is_local.rs) failed because
              condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                decls = decls(222, [trait CoreTrait <ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<FooStruct as Unit>::Assoc)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct { struct { } }], {}, {FooStruct}, {})
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
                &decls = decls(222, [trait CoreTrait <ty> ], [impl CoreTrait(CoreStruct)], [], [], [], [adt CoreStruct { struct { } }], {}, {}, {})
                &name = (adt CoreStruct)

            the rule "local rigid type" at (is_local.rs) failed because
              condition evaluted to false: `decls.is_local_adt_id(&a)`
                decls = decls(222, [trait CoreTrait <ty> ], [impl CoreTrait(CoreStruct)], [], [], [], [adt CoreStruct { struct { } }], {}, {}, {})
                &a = CoreStruct

            the rule "local trait" at (is_local.rs) failed because
              condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                decls = decls(222, [trait CoreTrait <ty> ], [impl CoreTrait(CoreStruct)], [], [], [], [adt CoreStruct { struct { } }], {}, {}, {})
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
