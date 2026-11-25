use a_mir_formality::{crates, FormalityTest};
use formality_core::test;

#[test]
fn hr_trait_bound_on_fn_with_body() {
    FormalityTest::new(crates![crate Foo {
        trait Trait<'a> {}
        fn foo<T>() -> () where for<'a> T: Trait<'a> {}
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn hr_trait_bound_at_body_lifetime() {
    FormalityTest::new(crates![crate Foo {
        trait Trait<'a> {}
        fn needs_one<'x, T>() -> () where T: Trait<'x> {}
        fn foo<T>() -> () where for<'a> T: Trait<'a> {
            exists<'r> {
                needs_one::<'r, T>();
            }
        }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn hr_trait_bound_at_lifetime_parameter() {
    FormalityTest::new(crates![crate Foo {
        trait Trait<'a> {}
        fn needs_one<'x, T>() -> () where T: Trait<'x> {}
        fn foo<'b, T>() -> () where for<'a> T: Trait<'a> {
            needs_one::<'b, T>();
        }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn hr_trait_bound_proves_hr_trait_bound() {
    FormalityTest::new(crates![crate Foo {
        trait Trait<'a> {}
        fn needs_all<T>() -> () where for<'a> T: Trait<'a> {}
        fn foo<T>() -> () where for<'a> T: Trait<'a> {
            needs_all::<T>();
        }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn trait_bound_does_not_prove_hr_trait_bound() {
    FormalityTest::new(crates![crate Foo {
        trait Trait<'a> {}
        fn needs_all<T>() -> () where for<'a> T: Trait<'a> {}
        fn foo<'b, T>() -> () where T: Trait<'b> {
            needs_all::<T>();
        }
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0277]: the trait bound `for<'a10> T01: Trait<'a10>` is not satisfied
          --> lib.rs
           |
        13 |     needs_all::<T01>();
           |                 ^^^ the trait `for<'a10> Trait<'a10>` is not implemented for `T01`
           |
        note: required by a bound in `needs_all`
          --> lib.rs
           |
         3 | pub fn needs_all<T00>() -> ()
           |        --------- required by a bound in this function
         4 | where
         5 |     for<'a10> T00: Trait<'a10>,
           |                    ^^^^^^^^^^^ required by this bound in `needs_all`

        For more information about this error, try `rustc --explain E0277`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        crates/formality-rust/src/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_1 = !lt_2, via: Trait(!ty_0, !lt_1), assumptions: {Trait(!ty_0, !lt_1)}, env: Env { variables: [!lt_1, !ty_0, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: true } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !lt_1, via: Trait(!ty_0, !lt_1), assumptions: {Trait(!ty_0, !lt_1)}, env: Env { variables: [!lt_1, !ty_0, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: true } }

        crates/formality-rust/src/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: !lt_2, via: Trait(!ty_0, !lt_1), assumptions: {Trait(!ty_0, !lt_1)}, env: Env { variables: [!lt_1, !ty_0, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: true } }

        the rule "trait implied bound" at (prove_predicate.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}

/// The bound puts no constraint on the loan's lifetime, so the loan ends after its last use.
#[test]
fn hr_trait_bound_loan_ends_after_last_use() {
    FormalityTest::new(crates![crate Foo {
        trait Trait<'a> {}
        fn first<'x, T>(v: &'x u32) -> &'x u32 where T: Trait<'x> {
            return v;
        }
        fn foo<T>() -> u32 where for<'a> T: Trait<'a> {
            let v: u32 = 1_u32;
            exists<'r> {
                let r: &'r u32 = first::<'r, T>(&'r v);
                let n: u32 = *r;
                v = 2_u32;
                return n;
            }
        }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn hr_trait_bound_loan_still_live() {
    FormalityTest::new(crates![crate Foo {
        trait Trait<'a> {}
        fn first<'x, T>(v: &'x u32) -> &'x u32 where T: Trait<'x> {
            return v;
        }
        fn foo<T>() -> u32 where for<'a> T: Trait<'a> {
            let v: u32 = 1_u32;
            exists<'r> {
                let r: &'r u32 = first::<'r, T>(&'r v);
                v = 2_u32;
                return *r;
            }
        }
    }])
    .rustc_err(expect_test::expect![[r#"
        error[E0506]: cannot assign to `v` because it is borrowed
          --> lib.rs
           |
        16 |         let mut r: &'_ u32 = first::<T00>(&v);
           |                                           -- `v` is borrowed here
        17 |         v = 2_u32;
           |         ^^^^^^^^^ `v` is assigned to here but it was already borrowed
        18 |         return *r;
           |                -- borrow later used here

        For more information about this error, try `rustc --explain E0506`.
        error: could not compile `Foo` (lib) due to 1 previous error
    "#]])
    .err(expect_test::expect![[r#"
        the rule "borrow of disjoint places" at (nll.rs) failed because
          condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
            &loan.place = v : u32
            &access.place = v : u32

        the rule "loan_cannot_outlive" at (nll.rs) failed because
          condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
            outlived_by_loan = {?lt_2}
            &lifetime.upcast() = ?lt_2

        the rule "write-indirect" at (nll.rs) failed because
          pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v`"#]]);
}

#[test]
fn hr_outlives_bound_gives_static() {
    FormalityTest::new(crates![crate Foo {
        fn needs_static<T>() -> () where T: 'static {}
        fn foo<T>() -> () where for<'a> T: 'a {
            needs_static::<T>();
        }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn hr_outlives_bound_gives_lifetime_parameter() {
    FormalityTest::new(crates![crate Foo {
        fn needs_one<'x, T>() -> () where T: 'x {}
        fn foo<'x, T>() -> () where for<'a> T: 'a {
            needs_one::<'x, T>();
        }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn hr_outlives_bound_proves_hr_outlives_bound() {
    FormalityTest::new(crates![crate Foo {
        fn needs_all<T>() -> () where for<'a> T: 'a {}
        fn foo<T>() -> () where for<'a> T: 'a {
            needs_all::<T>();
        }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

/// No binder here, but `for<'a> T: 'a` relies on the same rule.
#[test]
fn static_bound_gives_any_lifetime() {
    FormalityTest::new(crates![crate Foo {
        fn foo<'x, 'y>(v: &'x u32) -> &'y u32 where 'x: 'static {
            return v;
        }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}
