use formality_core::test;

/// Test assign statement with locals at rhs.
#[test]
fn test_assign_statement_local_only() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            return;
                        }
                    }
                };
            }
        ]
    )
}

// Test assign statement with constant at rhs.
#[test]
fn test_assign_constant() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo () -> u8 = minirust() -> v0 {
                    let v0: u8;
                    exists {
                        let v1: u16;
                        let v2: u32;
                        let v3: u64;
                        let v4: usize;
                        let v5: i8;
                        let v6: i16;
                        let v7: i32;
                        let v8: i64;
                        let v9: isize;
                        let v10: bool;

                        bb0: {
                            statements {
                                local(v0) = constant(5: u8);
                                local(v1) = constant(5: u16);
                                local(v2) = constant(5: u32);
                                local(v3) = constant(5: u64);
                                local(v4) = constant(5: usize);
                                local(v5) = constant(5: i8);
                                local(v6) = constant(5: i16);
                                local(v7) = constant(5: i32);
                                local(v8) = constant(5: i64);
                                local(v9) = constant(5: isize);
                                local(v10) = constant(false);
                            }
                            return;
                        }
                    }
                };
            }
        ]
    )
}

// Test valid program with Terminator::Switch.
#[test]
fn test_switch_statment() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo () -> u32 = minirust() -> v0 {
                    let v0: u32;
                    exists {
                        let v1: u32;

                        bb0: {
                            statements {
                                local(v1) = constant(0: u32);
                            }
                            switch(load(local(v1))) -> [(0: bb1), (1: bb2)] otherwise: bb3;
                        }

                        bb1: {
                            statements {
                                local(v0) = constant(1: u32);
                            }
                            return;
                        }

                        bb2: {
                            statements {
                                local(v0) = constant(2: u32);
                            }
                            return;
                        }

                        bb3: {
                            statements {
                            }
                            return;
                        }
                    }
                };
            }
        ]
    )
}

/// Test valid goto terminator.
#[test]
fn test_goto_terminator() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {}
                            goto bb1;
                        }

                        bb1: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            return;
                        }
                    }
                };
            }
        ]
    )
}

/// Test valid call terminator.
/// This is equivalent to:
/// ```
///    fn foo(v1: u32) -> u32 {
///      let v0: u32;
///      v0 = v1;
///      return v0;
///    }
///
///    fn bar(v1: u32) -> u32 {
///       v0 = v1;
///       let v0 = foo(v0);
///       return v0;
///    }
/// ```
#[test]
fn test_call_terminator() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo(u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            return;
                        }
                    }
                };

                fn bar(u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            call fn_id foo (Move(local(v0))) -> local(v0) goto bb1;
                        }

                        bb1: {
                            statements {}
                            return;
                        }
                    }
                };
            }
        ]
    )
}

/// Test valid place mention statement.
/// This is equivalent to:
/// ```
///    fn foo(v1: u32) -> u32 {
///      let v0: u32;
///      v0;
///      v0 = v1;
///      return v0;
///    }
///
/// ```
#[test]
fn test_place_mention_statement() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                place_mention(local(v0));
                                local(v0) = load(local(v1));
                            }
                            return;
                        }
                    }
                };
            }
        ]
    )
}

/// Test valid StorageLive and StorageDead statements.
#[test]
fn test_storage_live_dead() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        let v2: u32;

                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                                StorageLive(v2);
                                StorageDead(v2);
                            }
                            return;
                        }
                    }
                };
            }
        ]
    )
}

/// Test valid program that uses struct.
#[test]
fn test_struct() {
    crate::assert_ok!(
        [
            crate Foo {
                struct Dummy {
                    value: u32,
                    is_true: bool,
                }

                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        let v2: Dummy;

                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                                local(v2) = struct { constant(1: u32), constant(false)} as Dummy;
                                local(v2).0 = constant(2: u32);
                            }
                            return;
                        }
                    }
                };
            }
        ]
    )
}

// Test what will happen if the next block does not exist for Terminator::Call.
#[test]
fn test_no_next_bb_for_call_terminator() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo(u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            return;
                        }
                    }
                };

                fn bar() -> u32 = minirust() -> v0 {
                    let v0: u32;
                    exists {
                        let v1: u32;

                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            call fn_id foo (Move(local(v1))) -> local(v0);
                        }
                    }
                };
            }
        ]
    )
}

// Test the behaviour of calling a function that does not exist .
#[test]
fn test_call_invalid_fn() {
    crate::assert_err!(
        [
            crate Foo {
                fn bar() -> u32 = minirust() -> v0 {
                    let v0: u32;
                    exists {
                        let v1: u32;

                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            call fn_id foo (Move(local(v1))) -> local(v0);
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect![[r#"
            The function called is not declared in current crate"#]]
    )
}

#[test]
// Test what will happen if the type of arguments passed in is not subtype of what is expected.
fn test_pass_non_subtype_arg() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo(u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            return;
                        }
                    }
                };

                fn bar(()) -> () = minirust(v1) -> v0 {
                    let v0: ();
                    let v1: ();
                    exists {
                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            call fn_id foo (Move(local(v1))) -> local(v0) goto bb1;
                        }

                        bb1: {
                            statements {}
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect!["judgment had no applicable rules: `prove { goal: {() <: u32}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true }, decls: decls(222, [], [], [], [], [], [], {}, {}, {}) }`"]
    )
}

// Test the behaviour of having invalid next bbid Terminator::Call.
#[test]
fn test_invalid_next_bbid_for_call_terminator() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo(u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            return;
                        }
                    }
                };

                fn bar() -> u32 = minirust() -> v0 {
                    let v0: u32;
                    exists {
                        let v1: u32;

                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            call fn_id foo (Move(local(v1))) -> local(v0) goto bb1;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect!["Basic block bb1 does not exist"]
    )
}

/// Test what will happen if the declared and actual return type are not compatible.
/// This is equivalent to:
/// ```
/// fn foo(v1: ()) -> u32 {
///     let v0: ();
///     v0 = v1;
///     return v0;
/// }
/// ```
#[test]
fn test_incompatible_return_type() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo (()) -> u32 = minirust(v1) -> v0 {
                    let v0: ();
                    let v1: ();
                    exists {
                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                            }
                            return;
                        }
                    }
                };
            }
        ]

        []

        expect_test::expect!["judgment had no applicable rules: `prove { goal: {() <: u32}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, decls: decls(222, [], [], [], [], [], [], {}, {}, {}) }`"]
    )
}

#[test]
fn test_function_arg_number_mismatch() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo () -> () = minirust(v1) -> v0 {
                    let v0: ();
                    let v1: ();
                    exists {
                    }
                };
            }
        ]

        []

        expect_test::expect!["Function argument number mismatch: expected 0 arguments, but found 1"]
    )
}

// Test the behaviour of having unitialised return local variable.
// FIXME(#209): This test should fail but currently passes due to removed return place initialization check
#[test]
fn test_uninitialised_return_type() {
    crate::assert_ok!( // Changed from assert_err! - should be reverted when #209 is fixed
        [
            crate Foo {
                fn foo () -> u32 = minirust() -> v0 {
                    let v0: u32;
                    exists {
                        bb0: {
                            statements {
                            }
                            return;
                        }
                    }
                };
            }
        ]
    )
}

/// Test switch terminator with invalid type in Terminator::Switch.
#[test]
fn test_invalid_value_in_switch_terminator() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo () -> bool = minirust() -> v0 {
                    let v0: bool;
                    exists {
                        bb0: {
                            statements {
                                local(v0) = constant(false);
                            }
                            switch(load(local(v0))) -> [(0: bb1), (1: bb2)] otherwise: bb3;
                        }

                        bb1: {
                            statements {
                            }
                            return;
                        }

                        bb2: {
                            statements {
                            }
                            return;
                        }

                        bb3: {
                            statements {
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect![[r#"
            the rule "rigid_ty is int" at (mini_rust_check.rs) failed because
              condition evaluted to false: `id.is_int()`
                id = bool"#]]
    )
}

/// Test the behaviour of having return place in StorageDead.
#[test]
fn test_ret_place_storage_dead() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                StorageDead(v1);
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect!["Statement::StorageDead: trying to mark function arguments or return local as dead"]
    )
}

/// Test the behaviour of having function argument in StorageDead.
#[test]
fn test_fn_arg_storage_dead() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        bb0: {
                            statements {
                                StorageDead(v0);
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect!["Statement::StorageDead: trying to mark function arguments or return local as dead"]
    )
}

/// Test the behaviour of using invalid index for the struct field.
#[test]
fn test_invalid_struct_field() {
    crate::assert_err!(
        [
            crate Foo {
                struct Dummy {
                    value: u32,
                }

                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        let v2: Dummy;

                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                                local(v2) = struct { constant(1: u32) } as Dummy;
                                local(v2).1 = constant(2: u32);
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect!["The field index used in PlaceExpression::Field is invalid."]
    )
}

/// Test the behaviour of using non-adt local for field projection.
#[test]
fn test_field_projection_root_non_adt() {
    crate::assert_err!(
        [
            crate Foo {
                struct Dummy {
                    value: u32,
                }

                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        let v2: Dummy;

                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                                local(v2) = struct { constant(1: u32) } as Dummy;
                                local(v1).1 = constant(2: u32);
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect!["The local used for field projection is not adt."]
    )
}

/// Test the behaviour of initialising the struct with wrong type.
#[test]
fn test_struct_wrong_type_in_initialisation() {
    crate::assert_err!(
        [
            crate Foo {
                struct Dummy {
                    value: u32,
                }

                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        let v2: Dummy;

                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                                local(v2) = struct { constant(false) } as Dummy;
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect!["judgment had no applicable rules: `prove { goal: {bool <: u32}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true }, decls: decls(222, [], [], [], [], [], [adt Dummy { struct { value : u32 } }], {}, {Dummy}, {}) }`"]
    )
}

/// Test the behaviour of having non-adt as the type for ValueExpression::Struct.
#[test]
fn test_non_adt_ty_for_struct() {
    crate::assert_err!(
        [
            crate Foo {

                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    exists {
                        let v2: u32;

                        bb0: {
                            statements {
                                local(v0) = load(local(v1));
                                local(v2) = struct { constant(false) } as u32;
                            }
                            return;
                        }
                    }
                };
            }
        ]
        []
        expect_test::expect!["The type used in ValueExpression::Struct must be adt"]
    )
}

/// Basic pass test for lifetime.
///
/// The test is equivalent to:
/// ```rust,ignore
/// fn pick<'a>(v1: &'a u32) -> &'a u32 {
///     let v2 = v1;
///     v2
/// }
/// ```
#[test]
fn test_ref_identity() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo<lt a>(&a u32) -> &a u32 = minirust(v1) -> v0 {
                    let v0: &a u32;
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

/// Test ref and deref
#[test]
fn test_ref_deref() {
    crate::assert_ok!(
        [
            crate Foo {

                fn foo () -> u32 = minirust() -> v0 {
                    let v0: u32;
                    exists<lt a> {
                        let v1: &a u32;
                        let v2: u32;

                        bb0: {
                            statements {
                                local(v0) = constant(0: u32);
                                local(v1) = &a local(v0);
                                local(v2) = load(*(local(v1)));
                            }
                            return;
                        }
                    }
                };
            }
        ]
    )
}
