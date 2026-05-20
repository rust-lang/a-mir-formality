#![allow(non_snake_case)]
use a_mir_formality::{crates, FormalityTest};

#[test]
fn parser() {
    FormalityTest::new(crates![crate Foo {
        trait Baz where  cake  {}
    }])
    .err(expect_test::expect![[r#"
            × expected `:`
               ╭────
             1 │ [crate Foo { trait Baz where cake {} }]
               · ▲▲▲          ▲▲        ▲▲    ▲    ▲
               · │││          ││        ││    │    ╰── expected `:`
               · │││          ││        ││    ╰── while parsing WhereClause
               · │││          ││        │╰── while parsing TraitBoundData
               · │││          ││        ╰── while parsing TraitBinder
               · │││          │╰── while parsing Trait
               · │││          ╰── while parsing CrateItem
               · ││╰── while parsing Crate
               · │╰── while parsing Vec
               · ╰── while parsing Crates
               ╰────"#]])
}

#[test]
fn hello_world_fail() {
    FormalityTest::new(crates![crate Foo {
                trait Foo<T> where T: Bar<Self> {}

                trait Bar<T> where T: Baz {}

                trait Baz {}
            }]).err(expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ WellFormedTraitRef(Bar(!ty_0, !ty_1)), via: Bar(!ty_0, !ty_1), assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Baz(!ty_1), via: Bar(!ty_0, !ty_1), assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Baz(!ty_1), via: Place(?ty_2), assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0, ?ty_2, ?ty_3, ?ty_4], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Baz(!ty_1), via: Place(?ty_2), assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0, ?ty_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Baz(!ty_1), via: Place(?ty_2), assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: Baz(!ty_1), via: PlaceRead(?ty_2, ?ty_3), assumptions: {Bar(!ty_0, !ty_1)}, env: Env { variables: [!ty_1, !ty_0, ?ty_2, ?ty_3], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]])
}

#[test]
fn hello_world() {
    FormalityTest::new(crates![crate Foo {
        trait Foo<T> where T: Bar<Self>, Self: Baz {}

        trait Bar<T> where T: Baz {}

        trait Baz {}

        impl Baz for u32 {}

        impl Bar<u32> for u32 {}
        impl<T> Bar<T> for () where T: Baz {}
    }])
    .ok()
}

#[test]
fn basic_where_clauses_pass() {
    FormalityTest::new(crates![crate core {
        trait A<T> where T: B { }

        trait B { }

        trait WellFormed where for<T> u32: A<T> { }

        impl <T> B for T {}
    }])
    .ok()
}
#[test]
fn basic_where_clauses_fail() {
    FormalityTest::new(crates![crate core {
                trait A<T> where T: B { }

                trait B { }

                trait WellFormed where for<T> u32: A<T> { }
            }]).err(expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: @ WellFormedTraitRef(A(u32, !ty_1)), via: A(u32, ?ty_2), assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [!ty_1, ?ty_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: B(!ty_0), via: A(u32, ?ty_1), assumptions: {for <ty> A(u32, ^ty0_0)}, env: Env { variables: [!ty_0, ?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            the rule "trait implied bound" at (prove_wc.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`"#]])
}

#[test]
fn basic_adt_variant_dup() {
    FormalityTest::new(crates![crate Foo {
        enum Bar {
            Baz{},
            Baz{},
        }
    }])
    .err(expect_test::expect![[r#"
            the rule "adt" at (mod.rs) failed because
              variant "Baz" defined multiple times"#]])
}

#[test]
fn basic_adt_field_dup() {
    FormalityTest::new(crates![crate Foo {
        struct Bar {
            baz: (),
            baz: (),
        }
    }])
    .err(expect_test::expect![[r#"
            the rule "adt" at (mod.rs) failed because
              field "baz" of variant "struct" defined multiple times"#]])
}

#[test]
fn trait_items_with_duplicate_fn_names() {
    FormalityTest::new(crates![crate core {
        trait A {
            fn a() -> ();
            fn a() -> ();
        }
    }])
    .err(expect_test::expect![[r#"
            the rule "check trait" at (traits.rs) failed because
              the function name `a` is defined multiple times"#]]);
}

#[test]
fn trait_items_with_duplicate_associated_type_names() {
    FormalityTest::new(crates![crate core {
        trait A {
            type Assoc : [];
            type Assoc : [];
        }
    }])
    .err(expect_test::expect![[r#"
            the rule "check trait" at (traits.rs) failed because
              the associated type name `Assoc` is defined multiple times"#]]);
}

#[test]
fn crate_with_duplicate_item_names() {
    FormalityTest::new(crates![crate core {
        struct A {}

        enum A {}
    }])
    .err(expect_test::expect![[r#"
            the rule "check crate" at (mod.rs) failed because
              the item name `A` is defined multiple times"#]]);

    FormalityTest::new(crates![crate core {
        trait a {}

        trait a {}
    }])
    .err(expect_test::expect![[r#"
            the rule "check crate" at (mod.rs) failed because
              the trait name `a` is defined multiple times"#]]);

    FormalityTest::new(crates![crate core {
        fn a() -> () { trusted }

        fn a() -> () { trusted }
    }])
    .err(expect_test::expect![[r#"
            the rule "check crate" at (mod.rs) failed because
              the function name `a` is defined multiple times"#]]);

    FormalityTest::new(crates![crate core {
        trait a {}

        fn a() -> () { trusted }
    }])
    .ok();
}

#[test]
fn basic_impl_dup() {
    FormalityTest::new(crates![crate core {
        trait MyTrait {}
        struct MyStruct {}
        impl MyTrait for MyStruct {}
        impl MyTrait for MyStruct {}
    }])
    .err(expect_test::expect![[r#"
        the rule "check crate" at (mod.rs) failed because
          `impl MyTrait for MyStruct { }` is defined multiple times"#]]);
}

#[test]
fn impl_missing_required_fn() {
    FormalityTest::new(crates![crate core {
        trait Foo {
            fn bar(self_: u32) -> u32;
        }
        struct MyStruct {}
        impl Foo for MyStruct {}
    }])
    .err(expect_test::expect![[r#"
            the rule "check_trait_impl" at (impls.rs) failed because
              not all trait items implemented, missing: `bar`"#]]);
}

#[test]
fn impl_missing_one_of_two_fns() {
    FormalityTest::new(crates![crate core {
        trait Foo {
            fn bar(self_: u32) -> u32;
            fn baz(self_: u32) -> u32;
        }
        struct MyStruct {}
        impl Foo for MyStruct {
            fn bar(self_: u32) -> u32 {trusted}
        }
    }])
    .err(expect_test::expect![[r#"
            the rule "check_trait_impl" at (impls.rs) failed because
              not all trait items implemented, missing: `baz`"#]]);
}

#[test]
fn impl_with_default_fn_body_ok() {
    FormalityTest::new(crates![crate core {
        trait Foo {
            fn bar(self_: u32) -> u32 {trusted}
        }
        struct MyStruct {}
        impl Foo for MyStruct {}
    }])
    .ok();
}
