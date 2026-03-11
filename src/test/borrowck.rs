use formality_core::test;
/// Test the holding a shared reference to a local
/// integer variable prevents it from being incremented.
///
/// The test is equivalent to:
/// ```rust,ignore
/// fn mutate() -> i32 {
///     let mut i = 0;
///     let j = &mut i;
///     i = 1; // <-- ERROR
///     *j
/// }
/// ```
#[test]
fn mutable_ref_prevents_mutation() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo() -> i32 = minirust {
                    exists<'r0, 'r1> {
                        let v1: i32;
                        let v2: &mut 'r0 i32;

                        bb0: {
                            statements {
                                local(v1) = constant(0: i32);
                                local(v2) = &mut 'r1 local(v1);

                                // This should result in an error
                                local(v1) = constant(1: i32);

                                local(_return) = load(*(local(v2)));
                            }
                            return;
                        }
                    }
                };
            }
        ]

        [
        ]

        expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluted to false: `place_disjoint_from_place(&loan.place.to_place_expression(), &access.place)`
                &loan.place.to_place_expression() = local(v1)
                &access.place = local(v1)

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluted to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionKind::Deref(place_loaned_ref)` did not match value `local(v1)`"#]]
    )
}

/// Test the holding a shared reference to a local
/// integer variable prevents it from being incremented.
///
/// The test is equivalent to:
/// ```rust,ignore
/// fn mutate() -> i32 {
///     let mut i = 0;
///     let j = &i;
///     i = 1; // <-- ERROR
///     *j
/// }
/// ```
#[test]
fn shared_ref_prevents_mutation() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo() -> i32 = minirust {
                    exists<'r0, 'r1> {
                        let v1: i32;
                        let v2: &'r0 i32;

                        bb0: {
                            statements {
                                local(v1) = constant(0: i32);
                                local(v2) = &'r1 local(v1);

                                // This should result in an error
                                local(v1) = constant(1: i32);

                                local(_return) = load(*(local(v2)));
                            }
                            return;
                        }
                    }
                };
            }
        ]

        [
        ]

        expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluted to false: `place_disjoint_from_place(&loan.place.to_place_expression(), &access.place)`
                &loan.place.to_place_expression() = local(v1)
                &access.place = local(v1)

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluted to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionKind::Deref(place_loaned_ref)` did not match value `local(v1)`"#]]
    )
}

/// Test the holding a shared reference to a local
/// integer variable prevents it from being incremented.
///
/// The test is equivalent to:
/// ```rust,ignore
/// fn min_problem_case_3(m: &mut Map) -> &mut Map {
///     let n: &mut Map = &mut *m;
///     if some_condition() {
///         return n;
///     } else {
///         let o: &mut Map = &mut *m;
///         return o;
///     }
/// }
/// ```
#[test]
fn min_problem_case_3() {
    // This doesn't pass in Rust, but it does in our formulation.
    crate::assert_ok!(
        [
            crate Foo {
                struct Map { }

                fn min_problem_case_3<'a>(m: &mut 'a Map) -> &mut 'a Map
                = minirust {
                    exists<'r0, 'r1> {
                        let n: &mut 'r0 Map;
                        let o: &mut 'r1 Map;

                        bb0: {
                            statements {
                                local(n) = &mut 'r0 *(local(m));
                            }
                            goto bb1, bb2;
                        }

                        bb1: {
                            statements {
                                local(_return) = load(local(n));
                            }
                            return;
                        }

                        bb2: {
                            statements {
                                local(o) = &mut 'r1 *(local(m));
                                local(_return) = load(local(o));
                            }
                            return;
                        }
                    }
                };
            }
        ]
    )
}

/// A variant of Problem Case #3 which actually passes NLL
/// it doesn't exercise the interesting path.
///
/// The test is equivalent to:
/// ```rust,ignore
/// fn min_problem_case_3(m: &mut Map) -> &mut Map {
///     let n: &mut Map = &mut *m;
///     if some_condition() {
///     }
///     let o: &mut Map = &mut *m;
///     return o;
/// }
/// ```
#[test]
fn too_min_problem_case_3() {
    crate::assert_ok!(
        [
            crate Foo {
                struct Map { }

                fn min_problem_case_3<'a>(m: &mut 'a Map) -> &mut 'a Map
                = minirust {
                    exists<'r0, 'r1> {
                        let n: &mut 'r0 Map;
                        let o: &mut 'r1 Map;

                        bb0: {
                            statements {
                                local(n) = &mut 'r0 *(local(m));
                            }
                            goto bb1, bb2;
                        }

                        bb1: {
                            statements {
                            }
                            goto bb2;
                        }

                        bb2: {
                            statements {
                                local(o) = &mut 'r1 *(local(m));
                                local(_return) = load(local(o));
                            }
                            return;
                        }
                    }
                };
            }
        ]
    )
}

/// Upcasting from `'a` to `'b` errors because
/// there is no declared relationship.
#[formality_core::test]
fn undeclared_universal_region_relationship() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo<'a, 'b>(v1: &'a u32) -> &'b u32 = minirust {
                    exists<'r0> {
                        let v2: &'r0 u32;

                        bb0: {
                            statements {
                                local(v2) = load(local(v1));
                                local(_return) = load(local(v2));
                            }
                            return;
                        }
                    }
                };
            }
        ]

        [
        ]

        expect_test::expect!["judgment had no applicable rules: `borrow_check_block { loans_live_on_entry: {}, bb_id: bb0, fn_assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo <lt, lt> (v1 : &^lt0_0 u32) -> &^lt0_1 u32 = minirust{ exists <lt> { let v2 : &^lt0_0 u32 ; bb0 : { statements{ local(v2) = load(local(v1)) ; local(_return) = load(local(v2)) ; } return ; } } } ; }], env: Env { variables: [!lt_1, !lt_2, ?lt_3], bias: Soundness, pending: [], allow_pending_outlives: false }, output_ty: &!lt_2 u32, local_variables: {_return: &!lt_2 u32, v1: &!lt_1 u32, v2: &?lt_3 u32}, blocks: [bb0 : { statements{ local(v2) = load(local(v1)) ; local(_return) = load(local(v2)) ; } return ; }], ret_id: _return, input_args: [v1 : &!lt_1 u32], crate_id: Foo, decls: decls([crate Foo { fn foo <lt, lt> (v1 : &^lt0_0 u32) -> &^lt0_1 u32 = minirust{ exists <lt> { let v2 : &^lt0_0 u32 ; bb0 : { statements{ local(v2) = load(local(v1)) ; local(_return) = load(local(v2)) ; } return ; } } } ; }], 222) }, outlives: {}, stack: [] }`"]
    )
}

/// Same as `undeclared_universal_region_relationship`, but the function
/// loops forever and never returns. This tests whether the universal
/// outlives check fires even when there is no return terminator.
#[formality_core::test]
fn undeclared_universal_region_relationship_no_return() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo<'a, 'b>(v1: &'a u32) -> &'b u32 = minirust {
                    exists<'r0> {
                        let v2: &'r0 u32;

                        bb0: {
                            statements {
                                local(v2) = load(local(v1));
                                local(_return) = load(local(v2));
                            }
                            goto bb0;
                        }
                    }
                };
            }
        ]

        [
        ]

        expect_test::expect!["judgment had no applicable rules: `borrow_check_block { loans_live_on_entry: {}, bb_id: bb0, fn_assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo <lt, lt> (v1 : &^lt0_0 u32) -> &^lt0_1 u32 = minirust{ exists <lt> { let v2 : &^lt0_0 u32 ; bb0 : { statements{ local(v2) = load(local(v1)) ; local(_return) = load(local(v2)) ; } goto bb0 ; } } } ; }], env: Env { variables: [!lt_1, !lt_2, ?lt_3], bias: Soundness, pending: [], allow_pending_outlives: false }, output_ty: &!lt_2 u32, local_variables: {_return: &!lt_2 u32, v1: &!lt_1 u32, v2: &?lt_3 u32}, blocks: [bb0 : { statements{ local(v2) = load(local(v1)) ; local(_return) = load(local(v2)) ; } goto bb0 ; }], ret_id: _return, input_args: [v1 : &!lt_1 u32], crate_id: Foo, decls: decls([crate Foo { fn foo <lt, lt> (v1 : &^lt0_0 u32) -> &^lt0_1 u32 = minirust{ exists <lt> { let v2 : &^lt0_0 u32 ; bb0 : { statements{ local(v2) = load(local(v1)) ; local(_return) = load(local(v2)) ; } goto bb0 ; } } } ; }], 222) }, outlives: {}, stack: [] }`"]
    )
}

/// Upcasting from `'a` to `'b` is allowed because
/// there is a declared relationship.
#[formality_core::test]
fn declared_universal_region_relationship() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo<'a, 'b>(v1: &'a u32) -> &'b u32
                where
                    'a: 'b,
                = minirust {
                    exists<'r0> {
                        let v2: &'r0 u32;

                        bb0: {
                            statements {
                                local(v2) = load(local(v1));
                                local(_return) = load(local(v2));
                            }
                            return;
                        }
                    }
                };
            }
        ]
    )
}

/// Upcasting from `'a` to `'c` should be allowed because of
/// the transitive relationship.
#[formality_core::test]
fn declared_transitive_universal_region_relationship() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo<'a, 'b, 'c>(v1: &'a u32) -> &'c u32
                where
                    'a: 'b,
                    'b: 'c,
                = minirust {
                    exists<> {

                        bb0: {
                            statements {
                                local(_return) = load(local(v1));
                            }
                            return;
                        }
                    }
                };
            }
        ]
    )
}

/// Upcasting from `'a` to `'c` errors because of a missing
/// declared relationship to complete the transitive chain.
#[formality_core::test]
fn undeclared_transitive_universal_region_relationship() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo<'a, 'b, 'c>(v1: &'a u32) -> &'c u32
                where
                    'a: 'b,
                = minirust {
                    exists<> {

                        bb0: {
                            statements {
                                local(_return) = load(local(v1));
                            }
                            return;
                        }
                    }
                };
            }
        ]

        []

        expect_test::expect!["judgment had no applicable rules: `borrow_check_block { loans_live_on_entry: {}, bb_id: bb0, fn_assumptions: {!lt_1 : !lt_2}, env: TypeckEnv { program: [crate Foo { fn foo <lt, lt, lt> (v1 : &^lt0_0 u32) -> &^lt0_2 u32 where ^lt0_0 : ^lt0_1 = minirust{ exists { bb0 : { statements{ local(_return) = load(local(v1)) ; } return ; } } } ; }], env: Env { variables: [!lt_1, !lt_2, !lt_3], bias: Soundness, pending: [], allow_pending_outlives: false }, output_ty: &!lt_3 u32, local_variables: {_return: &!lt_3 u32, v1: &!lt_1 u32}, blocks: [bb0 : { statements{ local(_return) = load(local(v1)) ; } return ; }], ret_id: _return, input_args: [v1 : &!lt_1 u32], crate_id: Foo, decls: decls([crate Foo { fn foo <lt, lt, lt> (v1 : &^lt0_0 u32) -> &^lt0_2 u32 where ^lt0_0 : ^lt0_1 = minirust{ exists { bb0 : { statements{ local(_return) = load(local(v1)) ; } return ; } } } ; }], 222) }, outlives: {}, stack: [] }`"]
    )
}

// For `list: &mut Map`, borrow `&mut (*list).value` then assign to `list`.
#[test]
fn problem_case_4() {
    crate::assert_ok!(
        [
            crate Foo {
                struct Map {
                    value: u32,
                 }

                fn min_problem_case_4<'a>(list: &mut 'a Map, list2: &mut 'a Map) -> u32 = minirust {
                    exists<'r0> {
                        let num: &mut 'r0 u32;

                        bb0: {
                            statements {
                                local(_return) = constant(0 : u32);
                            }
                            goto bb1;
                        }

                        bb1: {
                            statements {
                                local(num) = &mut 'r0 *(local(list)).0;

                                local(list) = &mut 'a *(local(list2));

                                place_mention(local(num));
                            }
                            return;
                        }

                    }
                };
            }
        ]
    )
}

/// Test that StorageDead on a borrowed variable is an error.
///
/// The test is equivalent to:
/// ```rust,ignore
/// fn foo() -> i32 {
///     let v1: i32 = 0;
///     let v2: &i32 = &v1;
///     StorageDead(v1);  // ERROR: v1 is still borrowed
///     *v2
/// }
/// ```
#[test]
fn storage_dead_while_borrowed() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo() -> i32 = minirust {
                    exists<'r0> {
                        let v1: i32;
                        let v2: &'r0 i32;

                        bb0: {
                            statements {
                                local(v1) = constant(0: i32);
                                local(v2) = &'r0 local(v1);
                                StorageDead(v1);
                                local(_return) = load(*(local(v2)));
                            }
                            return;
                        }
                    }
                };
            }
        ]

        []

        expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluted to false: `place_disjoint_from_place(&loan.place.to_place_expression(), &access.place)`
                &loan.place.to_place_expression() = local(v1)
                &access.place = local(v1)

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluted to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionKind::Deref(place_loaned_ref)` did not match value `local(v1)`"#]]
    )
}

/// In this test, the write to `*(q.0)` is in fact safe,
/// but the borrow checker can't see it.
///
/// It is safe because, whichever value is returned by
/// `nondet()`, `p` and `q` remain disjoint.
/// But if you union the loans and assume both paths
/// may have been taken, then you get an error.
///
/// (In rustc, we do not get an error when `q` is just a
/// `&mut` local variable, we have to introduce the tuple,
/// so presumably something smart is happening around liveness
/// that I does not fully understand. --nikomatsakis)
#[test]
fn cfg_union_approx_cause_false_error() {
    /*
        #![allow(warnings)] // whiny rustc

    fn foo() -> u32 {
        let mut a = 0;
        let mut b = 0;
        let mut p;
        let mut q = (&mut a,);
        if nondet() {
            p = &mut a;
            q.0 = &mut b;
        } else {
            p = &mut b;
        }
        *(q.0) += 1;
        *p;
        // is there something that is "ok" if a is borrowed XOR b is borrowed?
        // but not if a is borrowed OR b is borrowed?
    }

    fn nondet() -> bool {
        true
    }
     */
    crate::assert_ok!(
        [
            crate Foo {
                fn foo () -> u32 = minirust {
                    exists<'l_p, 'l_q, 'loan_0, 'loan_1, 'loan_2, 'loan_3> {
                        let a: u32;
                        let b: u32;

                        // In Rustc, the 1-tuple is needed for some reason
                        // Niko does not 100% understand, else rustc is able to
                        // see that this program is safe.
                        let p: &mut 'l_p u32;
                        let q: &mut 'l_q u32;

                        bb0: {
                            statements {
                                local(a) = constant(0: u32);
                                local(b) = constant(0: u32);
                                local(q) = &mut 'loan_0 local(a);
                            }
                            goto bb1, bb2;
                        }

                        bb1: {
                            statements {
                                local(p) = &mut 'loan_1 local(a);
                                local(q) = &mut 'loan_2 local(b);
                            }
                            goto bb3;
                        }

                        bb2: {
                            statements {
                                local(p) = &mut 'loan_3 local(b);
                            }
                            goto bb3;
                        }

                        bb3: {
                            statements {
                                *(local(q)) = constant(1: u32);
                                local(_return) = load(*(local(p)));
                            }
                            return;
                        }
                    }
                };
            }
        ]
    )
}

/// Fail test for loan_cannot_outlive's "lifetime" rule.
/// This is equivalent to:
/// ```
/// fn foo() -> u32 {
///     let mut x: u32 = 22;
///     let p = &x;
///     let q = p;
///     x += 1;
///     println!("{q}");
///     return 0;
/// }
/// ```
#[formality_core::test]
fn loan_cannot_outlive_lifetime_fail() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo() -> u32 = minirust {

                    exists<'r0, 'r1> {
                        let x: u32;
                        let p: &'r0 u32;
                        let q: &'r1 u32;


                        bb0: {
                            statements {
                                local(x) = constant(22: u32);
                                local(p) = &'r0 local(x);
                                local(q) = load(local(p));
                                local(x) = constant(23: u32);
                                // we should pass if we remove the line below
                                place_mention(local(q));
                            }
                            goto bb1;
                        }


                        bb1: {
                            statements {
                                // not important, just return 0
                                local(_return) = constant(0: u32);
                            }
                            return;
                        }
                    }
                };
            }
        ]

        [
        ]

        expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluted to false: `place_disjoint_from_place(&loan.place.to_place_expression(), &access.place)`
                &loan.place.to_place_expression() = local(x)
                &access.place = local(x)

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluted to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_2

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionKind::Deref(place_loaned_ref)` did not match value `local(x)`"#]]
    )
}

/// Pass test for loan_cannot_outlive's "lifetime" rule.
/// This is equivalent to:
/// ```
/// fn foo() -> u32 {
///     let mut x: u32 = 22;
///     let p = &x;
///     let q = p;
///     x += 1;
///     return 0;
/// }
/// ```
#[formality_core::test]
fn loan_cannot_outlive_lifetime_pass() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo() -> u32 = minirust {

                    exists<'r0, 'r1> {
                        let x: u32;
                        let p: &'r0 u32;
                        let q: &'r1 u32;


                        bb0: {
                            statements {
                                local(x) = constant(22: u32);
                                local(p) = &'r0 local(x);
                                local(q) = load(local(p));
                                local(x) = constant(23: u32);
                            }
                            goto bb1;
                        }


                        bb1: {
                            statements {
                                local(_return) = constant(0: u32);
                            }
                            return;
                        }
                    }
                };
            }
        ]
    )
}
