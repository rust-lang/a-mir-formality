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
                fn foo() -> i32 {
                    exists<'r0, 'r1> {
                        let v1: i32 = 0 _ i32;
                        let v2: &mut 'r0 i32 = &mut 'r1 v1;
                        // This should result in an error
                        v1 = 1 _ i32;
                        return *v2;
                    }
                }
            }
        ]

        expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = v1 : i32
                &access.place = v1 : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v1`"#]]
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
                fn foo() -> i32 {
                    exists<'r0, 'r1> {
                        let v1: i32 = 0 _ i32;
                        let v2: &'r0 i32 = &'r1 v1;
                        v1 = 1 _ i32;
                        return *v2;
                    }
                }
            }
        ]

        expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = v1 : i32
                &access.place = v1 : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v1`"#]]
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

                fn min_problem_case_3<'a>(m: &mut 'a Map) -> &mut 'a Map {
                    exists<'r0, 'r1> {
                        let n: &mut 'r0 Map = &mut 'r0 *m;
                        if true {
                            return n;
                        } else {
                            let o: &mut 'r1 Map = &mut 'r1 *m;
                            return o;
                        }
                    }
                }
            }
        ]
    )
}

/// Test that dropping a borrowed variable is an error.
/// This is the expr-grammar equivalent of the old `storage_dead_while_borrowed` test.
///
/// ```rust,ignore
/// fn foo() -> i32 {
///     let v2: &i32;
///     {
///         let v1: i32 = 0;
///         v2 = &v1;     // borrow v1
///     }                  // v1 drops here — ERROR, still borrowed
///     *v2
/// }
/// ```
#[test]
fn drop_while_borrowed() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo() -> i32 {
                    exists<'r0, 'r1> {
                        let v2: &'r0 i32;
                        {
                            let v1: i32 = 0 _ i32;
                            v2 = &'r1 v1;
                        }
                        return *v2;
                    }
                }
            }
        ]

        expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = v1 : i32
                &access.place = v1 : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v1`"#]]
    )
}

/// Test that dropping a variable is fine when the borrow is no longer live.
///
/// ```rust,ignore
/// fn foo() -> i32 {
///     let result: i32;
///     {
///         let v1: i32 = 22;
///         let v2: &i32 = &v1;
///         result = *v2;          // use the borrow
///     }                           // v1 drops here — OK, borrow is dead
///     result
/// }
/// ```
#[test]
fn drop_after_borrow_dead() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo() -> i32 {
                    exists<'r0, 'r1> {
                        let result: i32;
                        {
                            let v1: i32 = 22 _ i32;
                            let v2: &'r0 i32 = &'r1 v1;
                            result = *v2;
                        }
                        return result;
                    }
                }
            }
        ]
    )
}

/// Test that dropping a mutably borrowed variable is an error.
///
/// ```rust,ignore
/// fn foo() -> i32 {
///     let v2: &mut i32;
///     {
///         let v1: i32 = 0;
///         v2 = &mut v1;  // mut borrow v1
///     }                   // v1 drops here — ERROR, still borrowed
///     *v2
/// }
/// ```
#[test]
fn drop_while_mutably_borrowed() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo() -> i32 {
                    exists<'r0, 'r1> {
                        let v2: &mut 'r0 i32;
                        {
                            let v1: i32 = 0 _ i32;
                            v2 = &mut 'r1 v1;
                        }
                        return *v2;
                    }
                }
            }
        ]

        expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = v1 : i32
                &access.place = v1 : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v1`"#]]
    )
}

/// Test that break out of a block drops locals in the exited scopes.
///
/// ```rust,ignore
/// fn foo() -> i32 {
///     let v2: &i32;
///     'a: {
///         let v1: i32 = 0;
///         v2 = &v1;         // borrow v1
///         break 'a;          // break exits 'a, dropping v1 — ERROR, v1 still borrowed
///     }
///     *v2
/// }
/// ```
#[test]
fn drop_on_break_while_borrowed() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo() -> i32 {
                    exists<'r0, 'r1> {
                        let v2: &'r0 i32;
                        'a: {
                            let v1: i32 = 0 _ i32;
                            v2 = &'r1 v1;
                            break 'a;
                        }
                        return *v2;
                    }
                }
            }
        ]

        expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = v1 : i32
                &access.place = v1 : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `v1`"#]]
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

                fn min_problem_case_3<'a>(m: &mut 'a Map) -> &mut 'a Map {
                    exists<'r0, 'r1> {
                        let n: &mut 'r0 Map = &mut 'r0 *m;
                        if true {
                        } else {
                        }
                        let o: &mut 'r1 Map = &mut 'r1 *m;
                        return o;
                    }
                }
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
                fn foo<'a, 'b>(v1: &'a u32) -> &'b u32 {
                    exists<'r0> {
                        let v2: &'r0 u32 = v1;
                        return v2;
                    }
                }
            }
        ]

        expect_test::expect!["crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }"]
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
                fn foo<'a, 'b>(v1: &'a u32, v2: &'b u32) -> () {
                    let output: &'b u32 = v2;
                    loop {
                        output = v1;
                    }
                }
            }
        ]

        expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_1, assumptions: {}, env: Env { variables: [!lt_0, !lt_1], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]
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
                {
                    exists<'r0> {
                        let v2: &'r0 u32 = v1;
                        return v2;
                    }
                }
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
                {
                    return v1;
                }
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
                {
                    return v1;
                }
            }
        ]

        expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: !lt_0 : !lt_2, via: !lt_0 : !lt_1, assumptions: {!lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }

            crates/formality-rust/src/prove/prove/prove/prove_outlives.rs:8:1: no applicable rules for prove_outlives { a: !lt_0, b: !lt_2, assumptions: {!lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]
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

                fn min_problem_case_4<'a>(list: &mut 'a Map, list2: &mut 'a Map) -> u32 {
                    exists<'r0> {
                        let num: &mut 'r0 u32 = &mut 'r0 (*list).value;
                        list = &mut 'a *list2;
                        num;
                        return 0 _ u32;
                    }
                }
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
#[ignore = "needs block scoping for storage dead semantics"]
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

        expect_test::expect![[r#"
            MaybeFnBody expected

            Caused by:
                0: = minirust
                       {
                           exists<'r0>
                           {
                               let v1: i32; let v2: &'r0 i32; bb0:
                               {
                                   statements
                                   {
                                       local(v1) = constant(0: i32); local(v2) = &'r0 local(v1);
                                       StorageDead(v1); local(_return) = load(*(local(v2)));
                                   } return;
                               }
                           }
                       };
                   }]
                1: failed to parse [crate Foo
                   {
                       fn foo() -> i32 = minirust
                       {
                           exists<'r0>
                           {
                               let v1: i32; let v2: &'r0 i32; bb0:
                               {
                                   statements
                                   {
                                       local(v1) = constant(0: i32); local(v2) = &'r0 local(v1);
                                       StorageDead(v1); local(_return) = load(*(local(v2)));
                                   } return;
                               }
                           }
                       };
                   }]"#]]
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
                fn foo () -> u32 {
                    exists<'l_p, 'l_q, 'loan_0, 'loan_1, 'loan_2, 'loan_3> {
                        let a: u32 = 0 _ u32;
                        let b: u32 = 0 _ u32;
                        // In Rustc, the 1-tuple is needed for some reason
                        // Niko does not 100% understand, else rustc is able to
                        // see that this program is safe.
                        let q: &mut 'l_q u32 = &mut 'loan_0 a;
                        let p: &mut 'l_p u32 = &mut 'loan_1 a;
                        if true {
                            p = &mut 'loan_1 a;
                            q = &mut 'loan_2 b;
                        } else {
                            p = &mut 'loan_3 b;
                        }
                        *q = 1 _ u32;
                        return *p;
                    }
                }
            }
        ]
    )
}

/// `continue` drops locals declared inside the loop body.
/// Borrowing a local and then continuing should fail if the
/// borrow escapes to a variable outside the loop.
#[test]
fn continue_drops_borrowed_local_false_edge() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo() -> i32 {
                    exists<'r0, 'r1> {
                        let r: &'r0 i32;
                        'a: loop {
                            let y: i32 = 0 _ i32;
                            r = &'r1 y;
                            continue 'a;
                        }
                        r; // only an error because of false edges, assumption that all loops terminate
                    }
                }
            }
        ]

        expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = y : i32
                &access.place = y : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `y`

            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = y : i32
                &access.place = y : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `y`"#]]
    );
}

/// `continue` drops locals declared inside the loop body.
/// Borrowing a local and then continuing should fail if the
/// borrow escapes to a variable outside the loop.
#[test]
fn continue_drops_borrowed_local_loop_carried() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo() -> i32 {
                    exists<'r0, 'r1> {
                        let x: i32 = 0 _ i32;
                        let r: &'r0 i32;
                        'a: loop {
                            r; // this *may* read from `y` in a previous iteration
                            let y: i32 = 0 _ i32;
                            r = &'r1 y;
                            continue 'a;
                        }
                    }
                }
            }
        ]

        expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = y : i32
                &access.place = y : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `y`

            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = y : i32
                &access.place = y : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `y`"#]]
    )
}

/// `break` drops locals declared inside the loop body.
/// Borrowing a local and then breaking should fail if the
/// borrow is used after the loop.
///
/// ```rust,compile_fail
/// fn foo() -> i32 {
///     let r: &i32;
///     'a: loop {
///         let x: i32 = 0;
///         r = &x;      // borrow x
///         break 'a;    // drops x while r is still live
///     }
///     *r
/// }
/// ```
#[test]
fn break_drops_borrowed_local() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo() -> i32 {
                    exists<'r0, 'r1> {
                        let r: &'r0 i32;
                        'a: loop {
                            let x: i32 = 0 _ i32;
                            r = &'r1 x;
                            break 'a;
                        }
                        return *r;
                    }
                }
            }
        ]

        expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = x : i32
                &access.place = x : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `x`

            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = x : i32
                &access.place = x : i32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `x`"#]]
    )
}

/// Locals declared inside a loop are properly scoped:
/// a borrow that doesn't escape is fine even with continue.
///
/// ```rust
/// fn foo() {
///     'a: loop {
///         let x: i32 = 0;
///         let r: &i32 = &x;
///         let _ = *r;     // use borrow
///         continue 'a;    // r is dead, x can be dropped
///     }
/// }
/// ```
#[test]
fn continue_drops_local_borrow_dead() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo() -> u32 {
                    exists<'r0, 'r1> {
                        'a: loop {
                            let x: i32 = 0 _ i32;
                            let r: &'r0 i32 = &'r1 x;
                            let _y: i32 = *r;
                            continue 'a;
                        }
                    }
                }
            }
        ]
    )
}

/// Locals declared inside a loop are properly scoped:
/// a borrow that doesn't escape is fine even with continue.
///
/// ```rust
/// fn foo() {
///     'a: loop {
///         let x: i32 = 0;
///         let r: &i32 = &x;
///         let _ = *r;     // use borrow
///         continue 'a;    // r is dead, x can be dropped
///     }
/// }
/// ```
#[test]
fn integer_in_outer_scope() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo() -> u32 {
                    'a: {
                        {
                            let 'a: v: i32 = 0 _ i32;
                        }
                    }
                }
            }
        ]
    )
}

/// Writing to a borrowed variable inside a loop before `continue`
/// should be an error when the borrow is live after the loop.
///
/// The liveness of `p` after the loop (used by `return *p`) should
/// propagate backward through the continue edge to the loop entry,
/// making `p` live at the point of `a = 23`. Since `p` borrows `a`,
/// the write to `a` conflicts with the live loan.
///
/// ```rust,compile_fail
/// fn foo() -> u32 {
///     let a: u32 = 22;
///     let p: &u32 = &a;
///     loop {
///         if true {
///             a = 23;       // ERROR: p's loan on a is live
///             continue;
///         }
///         break;
///     }
///     *p
/// }
/// ```
#[test]
fn write_to_borrowed_before_continue() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo() -> u32 {
                    exists<'r0, 'r1> {
                        let a: u32 = 22 _ u32;
                        let p: &'r0 u32 = &'r1 a;
                        'l: loop {
                            if true {
                                a = 23 _ u32;
                                continue 'l;
                            } else {
                                break 'l;
                            }
                        }
                        return *p;
                    }
                }
            }
        ]

        expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = a : u32
                &access.place = a : u32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `a`

            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = a : u32
                &access.place = a : u32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `a`"#]]
    )
}

/// Writing to a borrowed variable before a loop that might not execute
/// should be an error, because the borrow is live along the zero-iteration path.
///
/// ```rust,ignore
/// let a = 2;
/// let b = 2;
/// let p = &a;
/// a = 3;        // <-- error: p borrows a, and if the loop runs 0 times, p is still &a
/// loop {
///     p = &b;   // kills p's borrow of a, but only if the loop runs
/// }
/// *p
/// ```
#[test]
fn write_to_borrowed_before_zero_iteration_loop() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo() -> u32 {
                    exists<'r0, 'r1, 'r2> {
                        let a: u32 = 22 _ u32;
                        let b: u32 = 22 _ u32;
                        let p: &'r0 u32 = &'r1 a;
                        a = 23 _ u32;
                        'l: loop {
                            p = &'r2 b;
                            break 'l;
                        }
                        return *p;
                    }
                }
            }
        ]

        expect_test::expect![[r#"
            the rule "borrow of disjoint places" at (nll.rs) failed because
              condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = a : u32
                &access.place = a : u32

            the rule "loan_cannot_outlive" at (nll.rs) failed because
              condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                outlived_by_loan = {?lt_1, ?lt_2}
                &lifetime.upcast() = ?lt_1

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `a`"#]]
    )
}
