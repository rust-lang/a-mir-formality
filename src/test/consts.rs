#![allow(non_snake_case)]

#[test]
fn nonsense_rigid_const_bound() {
    crate::assert_err!(
        // This test is the short version of `generic_mismatch`, skipping
        // substituting and directly going to a wrong constant.
        [
            crate Foo {
                trait Foo where type_of_const true is u32 {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            check_trait(Foo)

            Caused by:
                0: prove_where_clauses_well_formed([type_of_const value(0, bool) is u32])
                1: judgment `prove { goal: {u32 = bool, @ wf(u32), @ wf(const value(0, bool))}, assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [trait Foo <ty> where {@ ConstHasType(value(0, bool) , u32)}], [], [], [], [], [], [], {Foo}, {}) }` failed at the following rule(s):
                     failed at (src/file.rs:LL:CC) because
                       judgment `prove_wc_list { goal: {u32 = bool, @ wf(u32), @ wf(const value(0, bool))}, assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                         the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                           judgment `prove_wc { goal: u32 = bool, assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                             the rule "assumption - relation" failed at step #1 (src/file.rs:LL:CC) because
                               judgment had no applicable rules: `prove_via { goal: u32 = bool, via: @ ConstHasType(value(0, bool) , u32), assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness } }`
                             the rule "eq" failed at step #0 (src/file.rs:LL:CC) because
                               judgment `prove_eq { a: u32, b: bool, assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                                 the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                                   judgment `prove_normalize { p: u32, assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                                     the rule "normalize-via-assumption" failed at step #1 (src/file.rs:LL:CC) because
                                       judgment had no applicable rules: `prove_normalize_via { goal: u32, via: @ ConstHasType(value(0, bool) , u32), assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness } }`
                                 the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                   judgment `prove_eq { a: bool, b: u32, assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                                     the rule "normalize-l" failed at step #0 (src/file.rs:LL:CC) because
                                       judgment `prove_normalize { p: bool, assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                                         the rule "normalize-via-assumption" failed at step #1 (src/file.rs:LL:CC) because
                                           judgment had no applicable rules: `prove_normalize_via { goal: bool, via: @ ConstHasType(value(0, bool) , u32), assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness } }`
                                     the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                                       cyclic proof attempt: `prove_eq { a: u32, b: bool, assumptions: {@ ConstHasType(value(0, bool) , u32)}, env: Env { variables: [], bias: Soundness } }`"#]]
    )
}

#[test]
fn ok() {
    crate::assert_ok!(
        //@check-pass
        [
            crate Foo {
                trait Foo<const C> where type_of_const C is bool {}
                trait Bar<const C> where type_of_const C is u32 {}

                impl<const C> Foo<const C> for u32 where type_of_const C is bool {}
            }
        ]

        expect_test::expect!["()"]
    )
}

#[test]
fn mismatch() {
    crate::assert_err!(
        [
            crate Foo {
                trait Foo<const C> where type_of_const C is bool {}

                impl Foo<const 42_u32> for u32 {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            check_trait_impl(impl Foo <const value(42, u32)> for u32 { })

            Caused by:
                judgment `prove { goal: {Foo(u32, const value(42, u32))}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl Foo(u32, const value(42, u32))], [], [], [], [], [], {Foo}, {}) }` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    judgment `prove_wc_list { goal: {Foo(u32, const value(42, u32))}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: Foo(u32, const value(42, u32)), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                          the rule "trait implied bound" failed at step #0 (src/file.rs:LL:CC) because
                            expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    )
}

#[test]
fn holds() {
    crate::assert_ok!(
        //@check-pass
        [
            crate Foo {
                trait Foo<const C> where type_of_const C is bool {}

                impl Foo<const true> for u32 {}
            }
        ]

        expect_test::expect!["()"]
    )
}

#[test]
fn rigid_const_bound() {
    crate::assert_ok!(
        // This test is the short version of `holds`, skipping
        // substituting and directly going to a rigid constant.
        //@check-pass
        [
            crate Foo {
                trait Foo where type_of_const true is bool {}
            }
        ]

        expect_test::expect!["()"]
    )
}

#[test]
fn generic_mismatch() {
    crate::assert_err!(
        [
            crate Foo {
                trait Foo<const C> where type_of_const C is bool {}

                impl<const C> Foo<const C> for u32 where type_of_const C is u32 {}
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            check_trait_impl(impl <const> Foo <const ^const0_0> for u32 where type_of_const ^const0_0 is u32 { })

            Caused by:
                judgment `prove { goal: {Foo(u32, const !const_0)}, assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness }, decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , u32)}], [], [], [], [], [], {Foo}, {}) }` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    judgment `prove_wc_list { goal: {Foo(u32, const !const_0)}, assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness } }` failed at the following rule(s):
                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: Foo(u32, const !const_0), assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness } }` failed at the following rule(s):
                          the rule "positive impl" failed at step #7 (src/file.rs:LL:CC) because
                            judgment `prove_after { constraints: Constraints { env: Env { variables: [!const_0, ?const_1], bias: Soundness }, known_true: true, substitution: {?const_1 => const !const_0} }, goal: {@ ConstHasType(?const_1 , bool)}, assumptions: {@ ConstHasType(!const_0 , u32)} }` failed at the following rule(s):
                              the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                                judgment `prove { goal: {@ ConstHasType(!const_0 , bool)}, assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness }, decls: decls(222, [trait Foo <ty, const> where {@ ConstHasType(^const0_1 , bool)}], [impl <const> Foo(u32, const ^const0_0) where {@ ConstHasType(^const0_0 , u32)}], [], [], [], [], [], {Foo}, {}) }` failed at the following rule(s):
                                  failed at (src/file.rs:LL:CC) because
                                    judgment `prove_wc_list { goal: {@ ConstHasType(!const_0 , bool)}, assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness } }` failed at the following rule(s):
                                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                        judgment `prove_wc { goal: @ ConstHasType(!const_0 , bool), assumptions: {@ ConstHasType(!const_0 , u32)}, env: Env { variables: [!const_0], bias: Soundness } }` failed at the following rule(s):
                                          the rule "const has ty" failed at step #0 (src/file.rs:LL:CC) because
                                            pattern `Some((_, const_ty))` did not match value `None`
                          the rule "trait implied bound" failed at step #0 (src/file.rs:LL:CC) because
                            expression evaluated to an empty collection: `decls.trait_invariants()`"#]]
    )
}

#[test]
fn multiple_type_of_const() {
    crate::assert_ok!(
        // This test is weird, but it's also not something rustc ever generates.
        // Types on const generics only get exactly one `type_of_const` bound.
        // Either way, it is still sound, because there is no constant that possibly
        // satisfies those bounds (similar to how there is no type that satisfies `Drop` + `Copy`).
        //@check-pass
        [
            crate Foo {
                trait Foo<const C> where type_of_const C is bool, type_of_const C is u32 {}
            }
        ]

        expect_test::expect!["()"]
    )
}
