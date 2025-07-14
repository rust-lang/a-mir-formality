
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
                            placeexpr_local(v0) = load(placeexpr_local(v1)); 
                        }
                        return;
                    }
                    
                };
            }
        ]
        expect_test::expect![["()"]]
    )
}



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
                            placeexpr_local(v0) = load(placeexpr_local(v1));
                        }
                        return;
                    }
                    
                };
            }
        ]
        expect_test::expect![["()"]]
    )
    
}

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
                            placeexpr_local(v0) = load(placeexpr_local(v1));
                        }
                        return;
                    }
                };

                fn bar() -> u32 = minirust() -> v0 {
                    let v0: u32;
                    let v1: u32;

                    bb0: {
                        statements {
                            placeexpr_local(v0) = load(placeexpr_local(v1));
                        } 
                        call fn_id foo (arg_place(placeexpr_local(v1))) -> placeexpr_local(v0) goto bb1;
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
                            place_mention(placeexpr_local(v0));
                            placeexpr_local(v0) = load(placeexpr_local(v1));
                        }
                        return;
                    }
                    
                };
            }
        ]
        expect_test::expect![["()"]]
    )
}


// Test the behaviour of assigning value that is not subtype of the place. 
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
                            placeexpr_local(v0) = load(placeexpr_local(v1)); 
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
                    judgment `prove_wc { goal: u32 <: (), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                      the rule "subtype - reflexive" failed at step #2 (src/file.rs:LL:CC) because
                        condition evaluted to false: `param1 == param2`
                          param1 = u32
                          param2 = ()"#]]
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
                            place_mention(placeexpr_local(v2));
                        }
                        return;
                    }
                    
                };
            }
        ]
        []
        expect_test::expect![[r#"
            PlaceExpression::Local: unknown local name"#]]
    )
}


// Test the behaviour of having invalid bb_id in goto. 
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

// Test what will happen if we call a function that does not exist .
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
                            placeexpr_local(v0) = load(placeexpr_local(v1));
                        } 
                        call fn_id foo (arg_place(placeexpr_local(v1))) -> placeexpr_local(v0);
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
// Test what will happen if the type of argument passed in is not subtype of what is expected.
fn test_pass_non_subtype_arg() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo(u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    
                    bb0: {
                        statements {
                            placeexpr_local(v0) = load(placeexpr_local(v1));
                        }
                        return;
                    }
                };

                fn bar(()) -> () = minirust(v1) -> v0 {
                    let v0: ();
                    let v1: ();

                    bb0: {
                        statements {
                            placeexpr_local(v0) = load(placeexpr_local(v1));
                        } 
                        call fn_id foo (arg_place(placeexpr_local(v1))) -> placeexpr_local(v0) goto bb1;
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
                    judgment `prove_wc { goal: () <: u32, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                      the rule "subtype - reflexive" failed at step #2 (src/file.rs:LL:CC) because
                        condition evaluted to false: `param1 == param2`
                          param1 = ()
                          param2 = u32"#]]
    )
}

// Test what will happen if the next block does not exist for Terminator::Call
// FIXME: we might want to allow this?
#[test]
fn test_no_next_bb_for_call_terminator() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo(u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    
                    bb0: {
                        statements {
                            placeexpr_local(v0) = load(placeexpr_local(v1));
                        }
                        return;
                    }
                };

                fn bar() -> u32 = minirust() -> v0 {
                    let v0: u32;
                    let v1: u32;

                    bb0: {
                        statements {
                            placeexpr_local(v0) = load(placeexpr_local(v1));
                        } 
                        call fn_id foo (arg_place(placeexpr_local(v1))) -> placeexpr_local(v0);
                    }
                    
                };
            }
        ]
        []
        expect_test::expect![[r#"
            There should be next block for Terminator::Call, but it does not exist!"#]]
    )

}

// Test what will happen if the fn's declared return type is not subtype of the local variable ret. 
#[test]
fn test_uncompatible_return_type() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo () -> () = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    
                    bb0: {
                        statements { 
                            placeexpr_local(v0) = load(placeexpr_local(v1));
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
                    judgment `prove_wc { goal: u32 <: (), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                      the rule "subtype - reflexive" failed at step #2 (src/file.rs:LL:CC) because
                        condition evaluted to false: `param1 == param2`
                          param1 = u32
                          param2 = ()"#]]
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