/// Test valid assign statement.
#[test]
fn test_assign_statement() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;

                    bb0: {
                        statements {
                            local(v0) = load(local(v1));
                        }
                        return;
                    }

                };
            }
        ]
        expect_test::expect![["()"]]
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

                };
            }
        ]
        expect_test::expect![["()"]]
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

                    bb0: {
                        statements {
                            local(v0) = load(local(v1));
                        }
                        return;
                    }
                };

                fn bar(u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;

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
                };
            }
        ]
        expect_test::expect![["()"]]
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

                    bb0: {
                        statements {
                            place_mention(local(v0));
                            local(v0) = load(local(v1));
                        }
                        return;
                    }

                };
            }
        ]
        expect_test::expect![["()"]]
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

                    bb0: {
                        statements {
                            local(v0) = load(local(v1));
                        }
                        return;
                    }
                };

                fn bar() -> u32 = minirust() -> v0 {
                    let v0: u32;
                    let v1: u32;

                    bb0: {
                        statements {
                            local(v0) = load(local(v1));
                        }
                        call fn_id foo (Move(local(v1))) -> local(v0);
                    }

                };
            }
        ]
        expect_test::expect![["()"]]
    )
}

/// Test the behaviour of assigning value that is not subtype of the place.
/// This is equivalent to:
/// ```
///    fn foo(v1: u32) -> () {
///      let v0: ();
///      v0 = v1;
///      return v0;
///    }
///    
/// ```
#[test]
fn test_invalid_assign_statement() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo (u32) -> () = minirust(v1) -> v0 {
                    let v0: ();
                    let v1: u32;

                    bb0: {
                        statements {
                            local(v0) = load(local(v1));
                        }
                        return;
                    }

                };
            }
        ]
        []
        expect_test::expect![[r#"
            judgment `prove { goal: {u32 <: ()}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
              failed at (src/file.rs:LL:CC) because
                judgment `prove_wc_list { goal: {u32 <: ()}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                    judgment had no applicable rules: `prove_wc { goal: u32 <: (), assumptions: {}, env: Env { variables: [], bias: Soundness } }`"#]]
    )
}

// Test the behaviour of having invalid local name in place mention.
#[test]
fn test_invalid_local_in_place_mention() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;

                    bb0: {
                        statements {
                            place_mention(local(v2));
                        }
                        return;
                    }

                };
            }
        ]
        []
        expect_test::expect!["PlaceExpression::Local: unknown local name `v2`"]
    )
}

// Test the behavior of having undeclared local_id in function argument.
#[test]
fn test_undeclared_local_in_function_arg() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;

                    bb0: {
                        statements {
                        }
                        return;
                    }

                };
            }
        ]
        []
        expect_test::expect!["Function argument v1 is not declared, consider declaring them with `let v1: type;`"]
    )
}

// Test the behavior of having undeclared local_id in return place.
#[test]
fn test_undeclared_local_in_return_place() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo () -> u32 = minirust() -> v0 {

                    bb0: {
                        statements {
                        }
                        return;
                    }

                };
            }
        ]
        []
        expect_test::expect!["Function return place v0 is not declared, consider declaring them with `let v0: type;`"]
    )
}

// Test the behaviour of having invalid bb_id in goto terminator.
#[test]
fn test_invalid_goto_bbid() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;

                    bb0: {
                        statements {}
                        goto bb1;
                    }

                };
            }
        ]
        []
        expect_test::expect![[r#"
            Basic block bb1 does not exist"#]]
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
                    let v1: u32;

                    bb0: {
                        statements {
                            local(v0) = load(local(v1));
                        }
                        call fn_id foo (Move(local(v1))) -> local(v0);
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

                    bb0: {
                        statements {
                            local(v0) = load(local(v1));
                        }
                        return;
                    }
                };

                fn bar(()) -> () = minirust(v1) -> v0 {
                    let v0: ();
                    let v1: ();

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

                };
            }
        ]
        []
        expect_test::expect![[r#"
            judgment `prove { goal: {() <: u32}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
              failed at (src/file.rs:LL:CC) because
                judgment `prove_wc_list { goal: {() <: u32}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                    judgment had no applicable rules: `prove_wc { goal: () <: u32, assumptions: {}, env: Env { variables: [], bias: Soundness } }`"#]]
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

                    bb0: {
                        statements {
                            local(v0) = load(local(v1));
                        }
                        return;
                    }
                };

                fn bar() -> u32 = minirust() -> v0 {
                    let v0: u32;
                    let v1: u32;

                    bb0: {
                        statements {
                            local(v0) = load(local(v1));
                        }
                        call fn_id foo (Move(local(v1))) -> local(v0) goto bb1;
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

                    bb0: {
                        statements {
                            local(v0) = load(local(v1));
                        }
                        return;
                    }

                };
            }
        ]

        []

        expect_test::expect![[r#"
            judgment `prove { goal: {() <: u32}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
              failed at (src/file.rs:LL:CC) because
                judgment `prove_wc_list { goal: {() <: u32}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                    judgment had no applicable rules: `prove_wc { goal: () <: u32, assumptions: {}, env: Env { variables: [], bias: Soundness } }`"#]]
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
                };
            }
        ]

        []

        expect_test::expect!["Function argument number mismatch: expected 0 arguments, but found 1"]
    )
}

// Test the behaviour of having unitialised return local variable.
#[test]
fn test_uninitialised_return_type() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo () -> u32 = minirust() -> v0 {
                    let v0: u32;

                    bb0: {
                        statements {
                        }
                        return;
                    }

                };
            }
        ]

        []

        expect_test::expect![[r#"
            The return local variable has not been initialized."#]]
    )
}
