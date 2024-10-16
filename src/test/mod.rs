#![allow(non_snake_case)]

mod coherence_orphan;
mod coherence_overlap;
mod consts;
mod decl_safety;
mod functions;

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
            check_trait(Foo)

            Caused by:
                0: prove_where_clauses_well_formed([!ty_2 : Bar <!ty_1>])
                1: judgment `prove { goal: {@ WellFormedTraitRef(Bar(!ty_0, !ty_1))}, assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0], bias: Soundness }, decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [], [], [], [], [], [], {Bar, Baz, Foo}, {}) }` failed at the following rule(s):
                     failed at (src/file.rs:LL:CC) because
                       judgment `prove_wc_list { goal: {@ WellFormedTraitRef(Bar(!ty_0, !ty_1))}, assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                         the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                           judgment `prove_wc { goal: @ WellFormedTraitRef(Bar(!ty_0, !ty_1)), assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                             the rule "trait well formed" failed at step #2 (src/file.rs:LL:CC) because
                               judgment `prove { goal: {Baz(!ty_1)}, assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0], bias: Soundness }, decls: decls(222, [trait Foo <ty, ty> where {Bar(^ty0_1, ^ty0_0)}, trait Bar <ty, ty> where {Baz(^ty0_1)}, trait Baz <ty> ], [], [], [], [], [], [], {Bar, Baz, Foo}, {}) }` failed at the following rule(s):
                                 failed at (src/file.rs:LL:CC) because
                                   judgment `prove_wc_list { goal: {Baz(!ty_1)}, assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                     the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                       judgment `prove_wc { goal: Baz(!ty_1), assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0], bias: Soundness } }` failed at the following rule(s):
                                         the rule "trait implied bound" failed at step #0 (src/file.rs:LL:CC) because
                                           expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    )
}

#[test]
fn hello_world() {
    crate::assert_ok!(
        //@check-pass
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

        expect_test::expect!["()"]
    )
}

#[test]
fn basic_where_clauses_pass() {
    crate::assert_ok!(
        //@check-pass
        [
            crate core {
                trait A<ty T> where T: B { }

                trait B { }

                trait WellFormed where for<ty T> u32: A<T> { }

                impl <ty T> B for T {}
            }
        ]

        expect_test::expect!["()"]
    )
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
            check_trait(WellFormed)

            Caused by:
                0: prove_where_clauses_well_formed([for <ty> u32 : A <^ty0_0>])
                1: judgment `prove { goal: {for <ty> @ WellFormedTraitRef(A(u32, ^ty0_0))}, assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [], [], [], [], [], [], {A, B, WellFormed}, {}) }` failed at the following rule(s):
                     failed at (src/file.rs:LL:CC) because
                       judgment `prove_wc_list { goal: {for <ty> @ WellFormedTraitRef(A(u32, ^ty0_0))}, assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                         the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                           judgment `prove_wc { goal: for <ty> @ WellFormedTraitRef(A(u32, ^ty0_0)), assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                             the rule "forall" failed at step #2 (src/file.rs:LL:CC) because
                               judgment `prove_wc { goal: @ WellFormedTraitRef(A(u32, !ty_1)), assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [!ty_1], bias: Soundness } }` failed at the following rule(s):
                                 the rule "trait well formed" failed at step #2 (src/file.rs:LL:CC) because
                                   judgment `prove { goal: {B(!ty_0)}, assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [!ty_0], bias: Soundness }, decls: decls(222, [trait A <ty, ty> where {B(^ty0_1)}, trait B <ty> , trait WellFormed <ty> where {for <ty> A(u32, ^ty0_0)}], [], [], [], [], [], [], {A, B, WellFormed}, {}) }` failed at the following rule(s):
                                     failed at (src/file.rs:LL:CC) because
                                       judgment `prove_wc_list { goal: {B(!ty_0)}, assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                         the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                           judgment `prove_wc { goal: B(!ty_0), assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [!ty_0], bias: Soundness } }` failed at the following rule(s):
                                             the rule "trait implied bound" failed at step #0 (src/file.rs:LL:CC) because
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
        //@check-pass
        [
            crate core {
                trait a {}

                fn a() -> () { trusted }
            }
        ]

        expect_test::expect!["()"]
    );
}
