use a_mir_formality::{crates, FormalityTest};

#[test]
fn i128_is_copy() {
    FormalityTest::new(crates![crate test {
        fn require_copy<T>() -> () where T: Copy { trusted }
        fn main() -> () { require_copy::<i128>(); }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn u128_is_copy() {
    FormalityTest::new(crates![crate test {
        fn require_copy<T>() -> () where T: Copy { trusted }
        fn main() -> () { require_copy::<u128>(); }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn trait_with_valid_fn() {
    FormalityTest::new(crates![
        crate core {
            trait A {
                fn a() -> ();
            }
        }
    ])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn trait_with_valid_associated_type() {
    FormalityTest::new(crates![
        crate core {
            trait A {
                type Assoc : [];
            }
        }
    ])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
#[ignore = "ensures bounds WF check not yet implemented, see FIXME(#228)"]
fn trait_with_ill_formed_where_clause() {
    FormalityTest::new(crates![
        crate core {
            trait A<T> where T: B {}
            trait B {}
            trait C {
                type Assoc : [ A<u32> ];
            }
        }
    ])
    .rustc_err(expect_test::expect![[r#""#]])
    .err(expect_test::expect![[r#"
            the rule "trait implied bound" at (prove_predicate.rs) failed because
              expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}

#[test]
fn associated_type_static_outlives_bound() {
    FormalityTest::new(crates![crate Foo {
        trait A { type Item: ['static]; }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn associated_type_outer_lifetime_outlives_bound() {
    FormalityTest::new(crates![crate Foo {
        trait A<'a> { type Item: ['a]; }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn associated_type_trait_and_outlives_bounds() {
    FormalityTest::new(crates![crate Foo {
        trait Bar {}
        trait A<'a> { type Item: [Bar, 'a]; }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn trait_with_lifetime_forall_on_assoc_type() {
    FormalityTest::new(crates![
        crate core {
            trait A { }
            trait B {
                type Item: [for<'a> A];
            }
        }
    ])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn trait_with_lifetime_forall_on_assoc_type_with_outer_params() {
    FormalityTest::new(crates![
        crate core {
            trait A<T> { }
            trait B<U> {
                type Item: [for<'a> A<U>];
            }
        }
    ])
    .skip_execute()
    .rustc_ok()
    .ok();
}

/// The higher-ranked bound applies to `Item`, not to the type implementing `B`.
/// `u32` implements `A` for every lifetime; `S` deliberately does not.
#[test]
fn higher_ranked_associated_type_bound_does_not_require_self_bound() {
    FormalityTest::new(crates![crate Foo {
        trait A<'a> {}
        trait B {
            type Item: [for<'a> A<'a>];
        }

        impl<'a> A<'a> for u32 {}
        struct S {}
        impl B for S {
            type Item = u32;
        }

        fn main() -> () {}
    }])
    .rustc_ok()
    .ok();
}
