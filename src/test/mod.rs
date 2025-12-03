#![allow(non_snake_case)]

mod coherence_orphan;
mod coherence_overlap;
mod consts;
mod decl_safety;
mod functions;
mod mir_fn_bodies;
mod well_formed_trait_ref;

#[test]
fn parser() {
    crate::assert_err!(
        [
            crate Foo {
                trait Baz where  cake  {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            expected `:`

            Caused by:
                0: {} }]
                1: failed to parse [crate Foo { trait Baz where cake {} }]"#]]
    )
}

#[test]
fn hello_world_fail() {
    crate::assert_err!(
        [
            crate Foo {
                trait Foo<ty T> where T: Bar<Self> {}

                trait Bar<ty T> where T: Baz {}

                trait Baz {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    )
}

#[test]
fn hello_world() {
    crate::assert_ok!(

        [
            crate Foo {
                trait Foo<ty T> where T: Bar<Self>, Self: Baz {}

                trait Bar<ty T> where T: Baz {}

                trait Baz {}

                impl Baz for u32 {}

                impl Bar<u32> for u32 {}
                impl<ty T> Bar<T> for () where T: Baz {}
            }
        ]
    )
}

#[test]
fn basic_where_clauses_pass() {
    crate::assert_ok!(
    [
        crate core {
            trait A<ty T> where T: B { }

            trait B { }

            trait WellFormed where for<ty T> u32: A<T> { }

            impl <ty T> B for T {}
        }
    ])
}
#[test]
fn basic_where_clauses_fail() {
    crate::assert_err!(
        [
            crate core {
                trait A<ty T> where T: B { }

                trait B { }

                trait WellFormed where for<ty T> u32: A<T> { }
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
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

        [ r#"variant "Baz" defined multiple times"#, ]

        expect_test::expect![[r#"variant "Baz" defined multiple times"#]]
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

        [ r#"field "baz" of variant "struct" defined multiple times"#, ]

        expect_test::expect![[r#"field "baz" of variant "struct" defined multiple times"#]]
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

        ["the function name `a` is defined multiple times",]

        expect_test::expect![[r#"
            check_trait(A)

            Caused by:
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

        ["the associated type name `Assoc` is defined multiple times",]

        expect_test::expect![[r#"
            check_trait(A)

            Caused by:
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

        ["the item name `A` is defined multiple times",]

        expect_test::expect![[r#"the item name `A` is defined multiple times"#]]
    );

    crate::assert_err!(
        [
            crate core {
                trait a {}

                trait a {}
            }
        ]

        ["the trait name `a` is defined multiple times",]

        expect_test::expect![[r#"the trait name `a` is defined multiple times"#]]
    );

    crate::assert_err!(
        [
            crate core {
                fn a() -> () { trusted }

                fn a() -> () { trusted }
            }
        ]

        ["the function name `a` is defined multiple times",]

        expect_test::expect![[r#"the function name `a` is defined multiple times"#]]
    );

    crate::assert_ok!(

        [
            crate core {
                trait a {}

                fn a() -> () { trusted }
            }
        ]
    );
}
