use formality_core::test;

/// Test assign statement with locals at rhs.
#[test]
fn test_assign_statement_local_only() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo (v1: u32) -> u32 {
                    return v1;
                }
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
                fn foo () -> u8 {
                    let v1: u16 = 5 _ u16;
                    let v2: u32 = 5 _ u32;
                    let v3: u64 = 5 _ u64;
                    let v4: usize = 5 _ usize;
                    let v5: i8 = 5 _ i8;
                    let v6: i16 = 5 _ i16;
                    let v7: i32 = 5 _ i32;
                    let v8: i64 = 5 _ i64;
                    let v9: isize = 5 _ isize;
                    let v10: bool = 0 _ bool;
                    return 5 _ u8;
                }
            }
        ]
    )
}

// Test valid program with Terminator::Switch.
#[test]
#[ignore = "needs switch/branching constructs in expr grammar"]
fn test_switch_statment() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo () -> u32 = minirust {
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
                                local(_return) = constant(1: u32);
                            }
                            return;
                        }

                        bb2: {
                            statements {
                                local(_return) = constant(2: u32);
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

/// Test returning a parameter (previously tested goto terminator in MIR).
#[test]
fn test_goto_terminator() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo (v1: u32) -> u32 {
                    return v1;
                }
            }
        ]
    )
}

/// Test cyclic control flow (a loop).
/// A silly but valid infinite loop that reads and writes a variable.
#[test]
fn test_cyclic_goto() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo() -> u32 {
                    let v0: u32 = 0 _ u32;
                    loop {
                        v0 = v0;
                    }
                }
            }
        ]
    )
}

/// Returns true (ExprData::True) type-checking coverage
#[test]
fn test_ret_true() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo () -> bool {
                    return true;
                }
            }
        ]
    )
}

// if / else (Stmt::If) bool condition, each arm returns a u32.
#[test]
fn if_else() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo (b: bool) -> u32 {
                    if b {
                        return 1 _ u32;
                    } else {
                        return 2 _ u32;
                    }
                }
            }
        ]
    )
}

// if / else (Stmt::If) with different return types.
#[test]
fn if_else_different_return_types() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo (b: bool) -> u32 {
                    if b {
                        return 1 _ u32;
                    } else {
                        return 0 _ bool;
                    }
                }
            }
        ]
        expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:19:1: no applicable rules for prove_normalize { p: bool, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:19:1: no applicable rules for prove_normalize { p: u32, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }"#]]
    )
}

/// Test valid call: bar calls foo.
#[test]
fn test_call_terminator() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo(v1: u32) -> u32 {
                    return v1;
                }

                fn bar(v1: u32) -> u32 {
                    let v0: u32 = foo(v1);
                    return v0;
                }
            }
        ]
    )
}

/// Test valid place mention statement.
/// This is equivalent to:
/// ```
///    fn foo(v1: u32) -> u32 {
///      _return;
///      return v1;
///    }
///
/// ```
#[test]
fn test_place_mention_statement() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo (v1: u32) -> u32 {
                    return v1;
                }
            }
        ]
    )
}

/// Test valid StorageLive and StorageDead statements.
#[test]
#[ignore = "needs StorageLive/StorageDead in expr grammar"]
fn test_storage_live_dead() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo (v1: u32) -> u32 = minirust {
                    exists {
                        let v2: u32;

                        bb0: {
                            statements {
                                local(_return) = load(local(v1));
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

                fn foo (v1: u32) -> u32 {
                    let v2: Dummy = Dummy { value: 1 _ u32, is_true: 0 _ bool };
                    v2.value = 2 _ u32;
                    return v1;
                }
            }
        ]
    )
}

// Test calling a function that does not exist.
#[test]
fn test_call_invalid_fn() {
    crate::assert_err!(
        [
            crate Foo {
                fn bar() -> u32 {
                    let v1: u32 = foo(0 _ u32);
                    return v1;
                }
            }
        ]
        expect_test::expect![[r#"
            the rule "fn-name" at (nll.rs) failed because
              no fn named `foo`

            the rule "local" at (nll.rs) failed because
              unknown local variable `foo`"#]]
    )
}

#[test]
// Test what will happen if the type of arguments passed in is not subtype of what is expected.
fn test_pass_non_subtype_arg() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo(v1: u32) -> u32 {
                    return v1;
                }

                fn bar(v1: ()) -> () {
                    let v0: () = foo(v1);
                    return v0;
                }
            }
        ]
        expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:19:1: no applicable rules for prove_normalize { p: (), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:19:1: no applicable rules for prove_normalize { p: u32, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }"#]]
    )
}

/// Test calling a generic function without turbofish (wrong number of type args: 0 vs 1).
#[test]
fn test_call_generic_fn_without_turbofish() {
    crate::assert_err!(
        [
            crate Foo {
                fn identity<T>(v1: T) -> T {
                    return v1;
                }

                fn bar(v1: u32) -> u32 {
                    let v0: u32 = identity(v1);
                    return v0;
                }
            }
        ]
        expect_test::expect![[r#"
            the rule "fn-name" at (nll.rs) failed because
              condition evaluated to false: `fn_decl.binder.len() == 0`

            the rule "local" at (nll.rs) failed because
              unknown local variable `identity`"#]]
    )
}

/// Test calling a generic function with correct turbofish.
#[test]
fn test_call_generic_fn_with_turbofish() {
    crate::assert_ok!(
        [
            crate Foo {
                fn identity<T>(v1: T) -> T {
                    return v1;
                }

                fn bar(v1: u32) -> u32 {
                    let v0: u32 = identity::<u32>(v1);
                    return v0;
                }
            }
        ]
    )
}

/// Test calling a generic function with wrong number of type args via turbofish.
#[test]
fn test_call_generic_fn_wrong_arity() {
    crate::assert_err!(
        [
            crate Foo {
                fn identity<T>(v1: T) -> T {
                    return v1;
                }

                fn bar(v1: u32) -> u32 {
                    let v0: u32 = identity::<u32, u32>(v1);
                    return v0;
                }
            }
        ]
        expect_test::expect![[r#"
            the rule "turbofish" at (nll.rs) failed because
              condition evaluated to false: `fn_decl.binder.len() == args.len()`"#]]
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
                fn foo (v1: ()) -> u32 {
                    return v1;
                }
            }
        ]

        expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:19:1: no applicable rules for prove_normalize { p: (), assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:19:1: no applicable rules for prove_normalize { p: u32, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }"#]]
    )
}

// Test the behaviour of having unitialised return local variable.
// FIXME(#209): This test should fail but currently passes due to removed return place initialization check
#[test]
#[ignore = "needs uninitialized return detection in expr grammar"]
fn test_uninitialised_return_type() {
    crate::assert_ok!( // Changed from assert_err! - should be reverted when #209 is fixed
        [
            crate Foo {
                fn foo () -> u32 = minirust {
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
#[ignore = "needs switch/branching constructs in expr grammar"]
fn test_invalid_value_in_switch_terminator() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo () -> bool = minirust {
                    exists {
                        bb0: {
                            statements {
                                local(_return) = constant(false);
                            }
                            switch(load(local(_return))) -> [(0: bb1), (1: bb2)] otherwise: bb3;
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
        expect_test::expect![[r#"
            MaybeFnBody expected

            Caused by:
                0: = minirust
                       {
                           exists
                           {
                               bb0:
                               {
                                   statements { local(_return) = constant(false); }
                                   switch(load(local(_return))) -> [(0: bb1), (1: bb2)]
                                   otherwise: bb3;
                               } bb1: { statements {} return; } bb2: { statements {} return; }
                               bb3: { statements {} return; }
                           }
                       };
                   }]
                1: failed to parse [crate Foo
                   {
                       fn foo() -> bool = minirust
                       {
                           exists
                           {
                               bb0:
                               {
                                   statements { local(_return) = constant(false); }
                                   switch(load(local(_return))) -> [(0: bb1), (1: bb2)]
                                   otherwise: bb3;
                               } bb1: { statements {} return; } bb2: { statements {} return; }
                               bb3: { statements {} return; }
                           }
                       };
                   }]"#]]
    )
}

/// Test the behaviour of having return place in StorageDead.
#[test]
#[ignore = "needs StorageDead in expr grammar"]
fn test_ret_place_storage_dead() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo (v1: u32) -> u32 = minirust {
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
        expect_test::expect![[r#"
            MaybeFnBody expected

            Caused by:
                0: = minirust
                       { exists { bb0: { statements { StorageDead(v1); } return; } } };
                   }]
                1: failed to parse [crate Foo
                   {
                       fn foo(v1: u32) -> u32 = minirust
                       { exists { bb0: { statements { StorageDead(v1); } return; } } };
                   }]"#]]
    )
}

/// Test the behaviour of having function argument in StorageDead.
#[test]
#[ignore = "needs StorageDead in expr grammar"]
fn test_fn_arg_storage_dead() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo (v1: u32) -> u32 = minirust {
                    exists {
                        bb0: {
                            statements {
                                StorageDead(_return);
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
                       { exists { bb0: { statements { StorageDead(_return); } return; } } };
                   }]
                1: failed to parse [crate Foo
                   {
                       fn foo(v1: u32) -> u32 = minirust
                       { exists { bb0: { statements { StorageDead(_return); } return; } } };
                   }]"#]]
    )
}

/// Test the behaviour of using invalid field name for the struct field.
#[test]
fn test_invalid_struct_field() {
    crate::assert_err!(
        [
            crate Foo {
                struct Dummy {
                    value: u32,
                }

                fn foo (v1: u32) -> u32 {
                    let v2: Dummy = Dummy { value: 1 _ u32 };
                    v2.nonexistent = 2 _ u32;
                    return v1;
                }
            }
        ]
        expect_test::expect![[r#"
            the rule "struct field" at (nll.rs) failed because
              condition evaluated to false: `field.name == *field_name`"#]]
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

                fn foo (v1: u32) -> u32 {
                    let v2: Dummy = Dummy { value: 1 _ u32 };
                    v1.value = 2 _ u32;
                    return v1;
                }
            }
        ]
        expect_test::expect![[r#"
            the rule "struct field" at (nll.rs) failed because
              pattern `(RigidTy { name: RigidName::AdtId(adt_id), parameters }, state)` did not match value `(u32, flow_state([scope(none, None, {}, None, [(v1, u32)], [v1 : u32]), scope(none, None, {}, None, [(v2, Dummy)], [v2 : Dummy])], point_flow_state({}, {}), {}, {}))`"#]]
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

                fn foo (v1: u32) -> u32 {
                    let v2: Dummy = Dummy { value: 0 _ bool };
                    return v1;
                }
            }
        ]
        expect_test::expect![[r#"
            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:19:1: no applicable rules for prove_normalize { p: bool, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }

            crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:19:1: no applicable rules for prove_normalize { p: u32, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: true } }"#]]
    )
}

/// Test the behaviour of having non-adt as the type for struct construction.
/// In the new expr syntax, using a scalar type name as a struct constructor
/// would be a parse error since AdtId and ScalarId are distinct.
/// We keep the intent of the test by using a made-up ADT name that doesn't exist.
#[test]
fn test_non_adt_ty_for_struct() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo (v1: u32) -> u32 {
                    let v2: u32 = Nonexistent { value: 0 _ bool };
                    return v1;
                }
            }
        ]
        expect_test::expect![[r#"
            the rule "struct" at (nll.rs) failed because
              no ADT named `Nonexistent`"#]]
    )
}

/// Test that the `false` literal type-checks as `bool`.
///
/// ```rust,ignore
/// fn foo() -> bool {
///     let v1: bool = false;
///     return v1;
/// }
/// ```
#[test]
fn test_false_literal() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo() -> bool {
                    let v1: bool = false;
                    return v1;
                }
            }
        ]
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
                fn foo<'a>(v1: &'a u32) -> &'a u32 {
                    exists<'r0> {
                        let v2: &'r0 u32 = v1;
                        return v2;
                    }
                }
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
                fn foo () -> u32 {
                    exists<'a> {
                        let v0: u32 = 0 _ u32;
                        let v1: &'a u32 = &'a v0;
                        let v2: u32 = *v1;
                        return v2;
                    }
                }
            }
        ]
    )
}

// ---- Scoping tests ----

/// `break` targeting a valid loop label should pass.
///
/// ```rust
/// fn foo() -> u32 {
///     'a: loop {
///         break 'a;
///     }
///     return 0;
/// }
/// ```
#[test]
fn test_break_valid_loop_label() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo() -> u32 {
                    'a: loop {
                        break 'a;
                    }
                    return 0 _ u32;
                }
            }
        ]
    )
}

/// `break` targeting a non-existent label should fail.
///
/// ```rust
/// fn foo() -> u32 {
///     loop {
///         break 'nonexistent; // error: no such label
///     }
/// }
/// ```
#[test]
fn test_break_nonexistent_label() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo() -> u32 {
                    loop {
                        break 'nonexistent;
                    }
                    return 0 _ u32;
                }
            }
        ]

        expect_test::expect![[r#"
            crates/formality-rust/src/check/borrow_check/nll.rs:165:1: no applicable rules for borrow_check_statement { state: flow_state([scope(none, None, {}, None, [], []), scope(none, None, {}, None, [], []), scope(none, None, {}, Some({}), [], []), scope(none, None, {}, None, [], [])], point_flow_state({}, {}), {}, {}), statement: break 'nonexistent ;, places_live_on_exit: {}, assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo () -> u32 { loop { break 'nonexistent ; } return 0 _ u32 ; } }], env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, output_ty: Some(u32), decls: decls([crate Foo { fn foo () -> u32 { loop { break 'nonexistent ; } return 0 _ u32 ; } }], 222) } }

            crates/formality-rust/src/check/borrow_check/nll.rs:165:1: no applicable rules for borrow_check_statement { state: flow_state([scope(none, None, {}, None, [], []), scope(none, None, {}, None, [], []), scope(none, None, {}, Some({}), [], []), scope(none, None, {}, None, [], [])], point_flow_state({}, {}), {}, {}), statement: break 'nonexistent ;, places_live_on_exit: {}, assumptions: {}, env: TypeckEnv { program: [crate Foo { fn foo () -> u32 { loop { break 'nonexistent ; } return 0 _ u32 ; } }], env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, output_ty: Some(u32), decls: decls([crate Foo { fn foo () -> u32 { loop { break 'nonexistent ; } return 0 _ u32 ; } }], 222) } }"#]]
    )
}

/// `continue` targeting a valid loop label should pass.
///
/// ```rust
/// fn foo() {
///     'a: loop {
///         continue 'a;
///     }
/// }
/// ```
#[test]
fn test_continue_valid_loop_label() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo() -> u32 {
                    'a: loop {
                        continue 'a;
                    }
                    return 0 _ u32;
                }
            }
        ]
    )
}

/// `continue` targeting a block (not a loop) should fail.
///
/// ```rust,compile_fail
/// fn foo() -> u32 {
///     'a: {
///         continue 'a; // error: can only continue loops
///     }
///     0
/// }
/// ```
#[test]
fn test_continue_block_label() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo() -> u32 {
                    'a: {
                        continue 'a;
                    }
                    return 0 _ u32;
                }
            }
        ]

        expect_test::expect![[r#"
            the rule "continue" at (nll.rs) failed because
              pattern `Some(places_live_on_continue)` did not match value `None`"#]]
    )
}

/// Test parenthesized place expression: `(*v).field`.
///
/// The parens around `*v1` are required so the deref applies before
/// the field projection (without them, `*v1.value` would try to
/// project first and then deref).
///
/// ```rust,ignore
/// fn foo(v1: &Pair) -> u32 {
///     let v2: u32 = (*v1).value;
///     v2
/// }
/// ```
#[test]
fn test_parens_place_expr() {
    crate::assert_ok!(
        [
            crate Foo {
                struct Pair {
                    value: u32,
                }

                fn foo<'a>(v1: &'a Pair) -> u32 {
                    exists<'r0> {
                        let v2: u32 = (*v1).value;
                        return v2;
                    }
                }
            }
        ]
    )
}

/// `break` targeting a block label (not a loop) should pass.
/// In Rust, you can `break` out of a labeled block.
///
/// ```rust
/// fn foo() -> u32 {
///     'a: {
///         break 'a;
///     }
///     0
/// }
/// ```
#[test]
fn test_break_block_label() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo() -> u32 {
                    'a: {
                        break 'a;
                    }
                    return 0 _ u32;
                }
            }
        ]
    )
}
