#![allow(non_snake_case)]

mod borrowck;
mod coherence_orphan;
mod coherence_overlap;
mod const_generics_rv_tsv_parse;
mod consts;
mod decl_safety;
mod functions;
mod mir_typeck;
mod references;
mod well_formed_trait_ref;

#[test]
fn parser() {
    crate::assert_err!(
        [
            crate Foo {
                trait Baz where  cake  {}
            }
        ]

        expect_test::expect![[r#"
            × expected `:`
               ╭────
             1 │ [crate Foo { trait Baz where cake {} }]
               · ▲▲▲          ▲▲        ▲▲    ▲▲   ▲
               · │││          ││        ││    ││   ╰── expected `:`
               · │││          ││        ││    │╰── while parsing WhereClauseData
               · │││          ││        ││    ╰── while parsing WhereClause
               · │││          ││        │╰── while parsing TraitBoundData
               · │││          ││        ╰── while parsing TraitBinder
               · │││          │╰── while parsing Trait
               · │││          ╰── while parsing CrateItem
               · ││╰── while parsing Crate
               · │╰── while parsing Vec
               · ╰── while parsing Crates
               ╰────"#]]
    )
}

#[test]
fn hello_world_fail() {
    crate::assert_err!(
        [
            crate Foo {
                trait Foo<T> where T: Bar<Self> {}

                trait Bar<T> where T: Baz {}

                trait Baz {}
            }
        ]

        expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ WellFormedTraitRef(Bar(!ty_0, !ty_1)), via: Bar(!ty_0, !ty_1), assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Baz(!ty_1), via: Bar(!ty_0, !ty_1), assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Baz(!ty_1), via: HasPlace(?ty_2), assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0, ?ty_2, ?ty_3, ?ty_4], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Baz(!ty_1), via: HasPlace(?ty_2), assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0, ?ty_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Baz(!ty_1), via: HasPlace(?ty_2), assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Baz(!ty_1), via: PlaceRead(?ty_2, ?ty_3), assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0, ?ty_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]
    )
}

#[test]
fn hello_world() {
    crate::assert_ok!(

        [
            crate Foo {
                trait Foo<T> where T: Bar<Self>, Self: Baz {}

                trait Bar<T> where T: Baz {}

                trait Baz {}

                impl Baz for u32 {}

                impl Bar<u32> for u32 {}
                impl<T> Bar<T> for () where T: Baz {}
            }
        ]
    )
}

#[test]
fn basic_where_clauses_pass() {
    crate::assert_ok!(
    [
        crate core {
            trait A<T> where T: B { }

            trait B { }

            trait WellFormed where for<T> u32: A<T> { }

            impl <T> B for T {}
        }
    ])
}
#[test]
fn basic_where_clauses_fail() {
    crate::assert_err!(
        [
            crate core {
                trait A<T> where T: B { }

                trait B { }

                trait WellFormed where for<T> u32: A<T> { }
            }
        ]

        expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ WellFormedTraitRef(A(u32, !ty_1)), via: A(u32, ?ty_2), assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: B(!ty_0), via: A(u32, ?ty_1), assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    )
}

#[test]
fn basic_adt_variant_dup() {
    crate::assert_err!(
        [
            crate Foo {
                enum Bar {
                    Baz{},
                    Baz{},
                }
            }
        ]

        expect_test::expect![[r#"
            the rule "adt" at (mod.rs) failed because
              variant "Baz" defined multiple times"#]]
    )
}

#[test]
fn basic_adt_field_dup() {
    crate::assert_err!(
        [
            crate Foo {
                struct Bar {
                    baz: (),
                    baz: (),
                }
            }
        ]

        expect_test::expect![[r#"
            the rule "adt" at (mod.rs) failed because
              field "baz" of variant "struct" defined multiple times"#]]
    )
}

#[test]
fn trait_items_with_duplicate_fn_names() {
    crate::assert_err!(
        [
            crate core {
                trait A {
                    fn a() -> ();
                    fn a() -> ();
                }
            }
        ]

        expect_test::expect![[r#"
            the rule "check trait" at (traits.rs) failed because
              the function name `a` is defined multiple times"#]]

    );
}

#[test]
fn trait_items_with_duplicate_associated_type_names() {
    crate::assert_err!(
        [
            crate core {
                trait A {
                    type Assoc : [];
                    type Assoc : [];
                }
            }
        ]

        expect_test::expect![[r#"
            the rule "check trait" at (traits.rs) failed because
              the associated type name `Assoc` is defined multiple times"#]]
    );
}

#[test]
fn crate_with_duplicate_item_names() {
    crate::assert_err!(
        [
            crate core {
                struct A {}

                enum A {}
            }
        ]

        expect_test::expect![[r#"
            the rule "check crate" at (mod.rs) failed because
              the item name `A` is defined multiple times"#]]
    );

    crate::assert_err!(
        [
            crate core {
                trait a {}

                trait a {}
            }
        ]

        expect_test::expect![[r#"
            the rule "check crate" at (mod.rs) failed because
              the trait name `a` is defined multiple times"#]]
    );

    crate::assert_err!(
        [
            crate core {
                fn a() -> () trusted;

                fn a() -> () trusted;
            }
        ]

        expect_test::expect![[r#"
            the rule "check crate" at (mod.rs) failed because
              the function name `a` is defined multiple times"#]]
    );

    crate::assert_ok!(

        [
            crate core {
                trait a {}

                fn a() -> () trusted;
            }
        ]
    );
}

#[test]
fn impl_missing_required_fn() {
    crate::assert_err!(
        [
            crate core {
                trait Foo {
                    fn bar(self_: u32) -> u32;
                }
                struct MyStruct {}
                impl Foo for MyStruct {}
            }
        ]

        expect_test::expect![[r#"
            the rule "check_trait_impl" at (impls.rs) failed because
              not all trait items implemented, missing: `bar`"#]]
    );
}

#[test]
fn impl_missing_one_of_two_fns() {
    crate::assert_err!(
        [
            crate core {
                trait Foo {
                    fn bar(self_: u32) -> u32;
                    fn baz(self_: u32) -> u32;
                }
                struct MyStruct {}
                impl Foo for MyStruct {
                    fn bar(self_: u32) -> u32 {trusted}
                }
            }
        ]

        expect_test::expect![[r#"
            the rule "check_trait_impl" at (impls.rs) failed because
              not all trait items implemented, missing: `baz`"#]]
    );
}

#[test]
fn impl_with_default_fn_body_ok() {
    crate::assert_ok!(
        [
            crate core {
                trait Foo {
                    fn bar(self_: u32) -> u32 {trusted}
                }
                struct MyStruct {}
                impl Foo for MyStruct {}
            }
        ]
    );
}
