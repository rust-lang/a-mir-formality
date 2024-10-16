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
            orphan_check_neg(impl ! CoreTrait for CoreStruct {})

            Caused by:
                judgment `prove { goal: {@ IsLocal(CoreTrait(CoreStruct))}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct ], [], {}, {}) }` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    judgment `prove_wc_list { goal: {@ IsLocal(CoreTrait(CoreStruct))}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: @ IsLocal(CoreTrait(CoreStruct)), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                          the rule "trait ref is local" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `is_local_trait_ref { goal: CoreTrait(CoreStruct), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                              the rule "local parameter" failed at step #1 (src/file.rs:LL:CC) because
                                judgment `is_local_parameter { goal: CoreStruct, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                                  the rule "fundamental rigid type" failed at step #0 (src/file.rs:LL:CC) because
                                    condition evaluted to false: `is_fundamental(&decls, &name)`
                                      &decls = decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct ], [], {}, {})
                                      &name = (adt CoreStruct)
                                  the rule "local rigid type" failed at step #0 (src/file.rs:LL:CC) because
                                    condition evaluted to false: `decls.is_local_adt_id(&a)`
                                      decls = decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct ], [], {}, {})
                                      &a = CoreStruct
                              the rule "local trait" failed at step #0 (src/file.rs:LL:CC) because
                                condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                                  decls = decls(222, [trait CoreTrait <ty> ], [], [impl ! CoreTrait(CoreStruct)], [], [], [adt CoreStruct ], [], {}, {})
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
            orphan_check(impl CoreTrait for <CoreStruct as Mirror>::Assoc { })

            Caused by:
                judgment `prove { goal: {@ IsLocal(CoreTrait(<CoreStruct as Mirror>::Assoc))}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<CoreStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt CoreStruct ], [], {}, {}) }` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    judgment `prove_wc_list { goal: {@ IsLocal(CoreTrait(<CoreStruct as Mirror>::Assoc))}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: @ IsLocal(CoreTrait(<CoreStruct as Mirror>::Assoc)), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                          the rule "trait ref is local" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `is_local_trait_ref { goal: CoreTrait(<CoreStruct as Mirror>::Assoc), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                              the rule "local parameter" failed at step #1 (src/file.rs:LL:CC) because
                                judgment `is_local_parameter { goal: <CoreStruct as Mirror>::Assoc, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                                  the rule "local parameter" failed at step #2 (src/file.rs:LL:CC) because
                                    judgment `is_local_parameter { goal: CoreStruct, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                                      the rule "fundamental rigid type" failed at step #0 (src/file.rs:LL:CC) because
                                        condition evaluted to false: `is_fundamental(&decls, &name)`
                                          &decls = decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<CoreStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt CoreStruct ], [], {}, {})
                                          &name = (adt CoreStruct)
                                      the rule "local rigid type" failed at step #0 (src/file.rs:LL:CC) because
                                        condition evaluted to false: `decls.is_local_adt_id(&a)`
                                          decls = decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<CoreStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt CoreStruct ], [], {}, {})
                                          &a = CoreStruct
                              the rule "local trait" failed at step #0 (src/file.rs:LL:CC) because
                                condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                                  decls = decls(222, [trait CoreTrait <ty> , trait Mirror <ty> ], [impl <ty> Mirror(^ty0_0), impl CoreTrait(<CoreStruct as Mirror>::Assoc)], [], [alias <ty> <^ty0_0 as Mirror>::Assoc = ^ty0_0], [], [adt CoreStruct ], [], {}, {})
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

        expect_test::expect!["()"]
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

        expect_test::expect!["()"]
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
            orphan_check(impl <ty> CoreTrait <FooStruct> for ^ty0_0 { })

            Caused by:
                judgment `prove { goal: {@ IsLocal(CoreTrait(!ty_0, FooStruct))}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness }, decls: decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(^ty0_0, FooStruct)], [], [], [], [adt FooStruct ], [], {}, {FooStruct}) }` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    judgment `prove_wc_list { goal: {@ IsLocal(CoreTrait(!ty_0, FooStruct))}, assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: @ IsLocal(CoreTrait(!ty_0, FooStruct)), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                          the rule "trait ref is local" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `is_local_trait_ref { goal: CoreTrait(!ty_0, FooStruct), assumptions: {}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                              the rule "local trait" failed at step #0 (src/file.rs:LL:CC) because
                                condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                                  decls = decls(222, [trait CoreTrait <ty, ty> ], [impl <ty> CoreTrait(^ty0_0, FooStruct)], [], [], [], [adt FooStruct ], [], {}, {FooStruct})
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
            orphan_check(impl CoreTrait for <FooStruct as Unit>::Assoc { })

            Caused by:
                judgment `prove { goal: {@ IsLocal(CoreTrait(<FooStruct as Unit>::Assoc))}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [trait CoreTrait <ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<FooStruct as Unit>::Assoc)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct ], [], {}, {FooStruct}) }` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    judgment `prove_wc_list { goal: {@ IsLocal(CoreTrait(<FooStruct as Unit>::Assoc))}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: @ IsLocal(CoreTrait(<FooStruct as Unit>::Assoc)), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                          the rule "trait ref is local" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `is_local_trait_ref { goal: CoreTrait(<FooStruct as Unit>::Assoc), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                              the rule "local parameter" failed at step #1 (src/file.rs:LL:CC) because
                                judgment `is_local_parameter { goal: <FooStruct as Unit>::Assoc, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                                  the rule "local parameter" failed at step #2 (src/file.rs:LL:CC) because
                                    judgment `is_local_parameter { goal: (), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                                      the rule "fundamental rigid type" failed at step #0 (src/file.rs:LL:CC) because
                                        condition evaluted to false: `is_fundamental(&decls, &name)`
                                          &decls = decls(222, [trait CoreTrait <ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<FooStruct as Unit>::Assoc)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct ], [], {}, {FooStruct})
                                          &name = tuple(0)
                              the rule "local trait" failed at step #0 (src/file.rs:LL:CC) because
                                condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                                  decls = decls(222, [trait CoreTrait <ty> , trait Unit <ty> ], [impl <ty> Unit(^ty0_0), impl CoreTrait(<FooStruct as Unit>::Assoc)], [], [alias <ty> <^ty0_0 as Unit>::Assoc = ()], [], [adt FooStruct ], [], {}, {FooStruct})
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
            orphan_check(impl CoreTrait for CoreStruct { })

            Caused by:
                judgment `prove { goal: {@ IsLocal(CoreTrait(CoreStruct))}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [trait CoreTrait <ty> ], [impl CoreTrait(CoreStruct)], [], [], [], [adt CoreStruct ], [], {}, {}) }` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    judgment `prove_wc_list { goal: {@ IsLocal(CoreTrait(CoreStruct))}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: @ IsLocal(CoreTrait(CoreStruct)), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                          the rule "trait ref is local" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `is_local_trait_ref { goal: CoreTrait(CoreStruct), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                              the rule "local parameter" failed at step #1 (src/file.rs:LL:CC) because
                                judgment `is_local_parameter { goal: CoreStruct, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                                  the rule "fundamental rigid type" failed at step #0 (src/file.rs:LL:CC) because
                                    condition evaluted to false: `is_fundamental(&decls, &name)`
                                      &decls = decls(222, [trait CoreTrait <ty> ], [impl CoreTrait(CoreStruct)], [], [], [], [adt CoreStruct ], [], {}, {})
                                      &name = (adt CoreStruct)
                                  the rule "local rigid type" failed at step #0 (src/file.rs:LL:CC) because
                                    condition evaluted to false: `decls.is_local_adt_id(&a)`
                                      decls = decls(222, [trait CoreTrait <ty> ], [impl CoreTrait(CoreStruct)], [], [], [], [adt CoreStruct ], [], {}, {})
                                      &a = CoreStruct
                              the rule "local trait" failed at step #0 (src/file.rs:LL:CC) because
                                condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                                  decls = decls(222, [trait CoreTrait <ty> ], [impl CoreTrait(CoreStruct)], [], [], [], [adt CoreStruct ], [], {}, {})
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

    expect_test::expect!["()"]
    )
}
