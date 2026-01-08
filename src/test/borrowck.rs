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
                fn foo() -> i32 = minirust() -> v0 {
                    let v0: i32;

                    exists<lt r0, lt r1> {
                        let v1: i32;
                        let v2: &mut r0 i32;

                        bb0: {
                            statements {
                                local(v1) = constant(0: i32);
                                local(v2) = &mut r1 local(v1);

                                // This should result in an error
                                local(v1) = constant(1: i32);

                                local(v0) = load(*(local(v2)));
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
              condition evaluted to false: `typed_place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = local(v1) : i32
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
                fn foo() -> i32 = minirust() -> v0 {
                    let v0: i32;

                    exists<lt r0, lt r1> {
                        let v1: i32;
                        let v2: &r0 i32;

                        bb0: {
                            statements {
                                local(v1) = constant(0: i32);
                                local(v2) = &r1 local(v1);

                                // This should result in an error
                                local(v1) = constant(1: i32);

                                local(v0) = load(*(local(v2)));
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
              condition evaluted to false: `typed_place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = local(v1) : i32
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
    crate::assert_err!(
        [
            crate Foo {
                struct Map { }

                fn min_problem_case_3<lt a>(&mut a Map) -> &mut a Map
                = minirust(m) -> ret {
                    let ret: &mut a Map;
                    let m: &mut a Map;

                    exists<lt r0, lt r1> {
                        let n: &mut r0 Map;
                        let o: &mut r1 Map;

                        bb0: {
                            statements {
                                local(n) = &mut r0 *(local(m));
                            }
                            goto bb1, bb2;
                        }

                        bb1: {
                            statements {
                                local(ret) = load(local(n));
                            }
                            return;
                        }

                        bb2: {
                            statements {
                                local(o) = &mut r1 *(local(m));
                                local(ret) = load(local(o));
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
              condition evaluted to false: `typed_place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = *(local(m) : &mut !lt_1 Map) : Map
                &access.place = *(local(m))

            the rule "loan_not_required_by_universal_regions" at (nll.rs) failed because
              condition evaluted to false: `outlived_by_loan.iter().all(|p| match p
              {
                  Parameter::Ty(_) => false, Parameter::Lt(lt) => match lt.data()
                  {
                      LtData::Static => false, LtData::Variable(Variable::UniversalVar(_))
                      => false, LtData::Variable(Variable::ExistentialVar(_)) => true,
                      LtData::Variable(Variable::BoundVar(_)) =>
                      panic!("cannot outlive a bound var"),
                  }, Parameter::Const(_) => panic!("cannot outlive a constant"),
              })`

            the rule "write-indirect" at (nll.rs) failed because
              pattern `TypedPlaceExpressionKind::Deref(place_loaned_ref)` did not match value `local(m)`

            the rule "write-indirect" at (nll.rs) failed because
              condition evaluted to false: `place_accessed.is_prefix_of(&place_loaned_ref.to_place_expression())`
                place_accessed = *(local(m))
                &place_loaned_ref.to_place_expression() = local(m)"#]]
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

                fn min_problem_case_3<lt a>(&mut a Map) -> &mut a Map
                = minirust(m) -> ret {
                    let ret: &mut a Map;
                    let m: &mut a Map;

                    exists<lt r0, lt r1> {
                        let n: &mut r0 Map;
                        let o: &mut r1 Map;

                        bb0: {
                            statements {
                                local(n) = &mut r0 *(local(m));
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
                                local(o) = &mut r1 *(local(m));
                                local(ret) = load(local(o));
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
                fn foo<lt a, lt b>(&a u32) -> &b u32 = minirust(v1) -> v0 {
                    let v0: &b u32;
                    let v1: &a u32;

                    exists<lt r0> {
                        let v2: &r0 u32;

                        bb0: {
                            statements {
                                local(v2) = load(local(v1));
                                local(v0) = load(local(v2));
                            }
                            return;
                        }
                    }
                };
            }
        ]

        [
        ]

        expect_test::expect!["judgment had no applicable rules: `verify_universal_outlives { env: TypeckEnv { program: [crate Foo { fn foo <lt, lt> (&^lt0_0 u32) -> &^lt0_1 u32 = minirust(v1) -> v0 { let v0 : &^lt0_1 u32 ; let v1 : &^lt0_0 u32 ; exists <lt> { let v2 : &^lt0_0 u32 ; bb0 : { statements{ local(v2) = load(local(v1)) ; local(v0) = load(local(v2)) ; } return ; } } } ; }], env: Env { variables: [!lt_1, !lt_2, ?lt_3], bias: Soundness, pending: [], allow_pending_outlives: false }, output_ty: &!lt_2 u32, local_variables: {v0: &!lt_2 u32, v1: &!lt_1 u32, v2: &?lt_3 u32}, blocks: [bb0 : { statements{ local(v2) = load(local(v1)) ; local(v0) = load(local(v2)) ; } return ; }], ret_id: v0, declared_input_tys: [&!lt_1 u32], crate_id: Foo, fn_args: [v1], decls: decls(222, [], [], [], [], [], [], {}, {}, {}) }, fn_assumptions: {}, outlives: {PendingOutlives { location: Location, a: !lt_1, b: ?lt_3 }, PendingOutlives { location: Location, a: ?lt_3, b: !lt_2 }} }`"]
    )
}

/// Upcasting from `'a` to `'b` is allowed because
/// there is a declared relationship.
#[formality_core::test]
fn declared_universal_region_relationship() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo<lt a, lt b>(&a u32) -> &b u32
                where
                    a: b,
                = minirust(v1) -> v0 {
                    let v0: &b u32;
                    let v1: &a u32;

                    exists<lt r0> {
                        let v2: &r0 u32;

                        bb0: {
                            statements {
                                local(v2) = load(local(v1));
                                local(v0) = load(local(v2));
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
#[should_panic] // FIXME(#221)
fn declared_transitive_universal_region_relationship() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo<lt a, lt b, lt c>(&a u32) -> &c u32
                where
                    a: b,
                    b: c,
                = minirust(v1) -> v0 {
                    let v0: &b u32;
                    let v1: &a u32;

                    exists<lt r0> {
                        let v2: &r0 u32;

                        bb0: {
                            statements {
                                local(v2) = load(local(v1));
                                local(v0) = load(local(v2));
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
                fn foo<lt a, lt b, lt c>(&a u32) -> &c u32
                where
                    a: b,
                = minirust(v1) -> v0 {
                    let v0: &b u32;
                    let v1: &a u32;

                    exists<lt r0> {
                        let v2: &r0 u32;

                        bb0: {
                            statements {
                                local(v2) = load(local(v1));
                                local(v0) = load(local(v2));
                            }
                            return;
                        }
                    }
                };
            }
        ]

        []

        expect_test::expect!["judgment had no applicable rules: `prove { goal: {&!lt_1 u32 <: &!lt_2 u32}, assumptions: {!lt_0 : !lt_1}, env: Env { variables: [!lt_0, !lt_1, !lt_2], bias: Soundness, pending: [], allow_pending_outlives: false }, decls: decls(222, [], [], [], [], [], [], {}, {}, {}) }`"]
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

                fn min_problem_case_4<lt a>(&mut a Map, &mut a Map) -> u32 = minirust(list, list2) -> ret {
                    let list: &mut a Map;
                    let list2: &mut a Map;
                    let ret: u32;

                    exists<lt r0> {
                        let num: &mut r0 u32;

                        bb0: {
                            statements {
                                local(ret) = constant(0 : u32);
                            }
                            goto bb1;
                        }

                        bb1: {
                            statements {
                                local(num) = &mut r0 *(local(list)).0;

                                // FIXME: We don't account for "kills" and so we forbid
                                // this assignment as `list` is still borrowed.
                                local(list) = &mut a *(local(list2));

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
                fn foo() -> i32 = minirust() -> v0 {
                    let v0: i32;

                    exists<lt r0> {
                        let v1: i32;
                        let v2: &r0 i32;

                        bb0: {
                            statements {
                                local(v1) = constant(0: i32);
                                local(v2) = &r0 local(v1);
                                StorageDead(v1);
                                local(v0) = load(*(local(v2)));
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
              condition evaluted to false: `typed_place_disjoint_from_place(&loan.place, &access.place)`
                &loan.place = local(v1) : i32
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
/// (In rustc, we do not get an error when `p` is just a
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
                fn foo () -> u32 = minirust() -> v0 {
                    let v0: u32;
                    exists<lt l_p, lt l_q, lt loan_0, lt loan_1, lt loan_2, lt loan_3> {
                        let a: u32;
                        let b: u32;

                        // In Rustc, the 1-tuple is needed for some reason
                        // Niko does not 100% understand, else rustc is able to
                        // see that this program is safe.
                        let p: &mut l_p u32;
                        let q: &mut l_q u32;

                        bb0: {
                            statements {
                                local(a) = constant(0: u32);
                                local(b) = constant(0: u32);
                                local(q) = &mut loan_0 local(a);
                            }
                            goto bb1, bb2;
                        }

                        bb1: {
                            statements {
                                local(p) = &mut loan_1 local(a);
                                local(q) = &mut loan_2 local(b);
                            }
                            goto bb3;
                        }

                        bb2: {
                            statements {
                                local(p) = &mut loan_3 local(b);
                            }
                            goto bb3;
                        }

                        bb3: {
                            statements {
                                *(local(q)) = constant(1: u32);
                                local(v0) = load(*(local(p)));
                            }
                            return;
                        }
                    }
                };
            }
        ]
    )
}
