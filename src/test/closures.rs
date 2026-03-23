use formality_core::test;

/// Test: closure definition with a trusted body passes checking.
#[test]
fn test_closure_def_trusted() {
    crate::assert_ok!(
        [
            crate Foo {
                closure my_closure [] () -> u32 { trusted }
            }
        ]
    )
}

/// Test: closure definition with a by_value capture and trusted body.
#[test]
fn test_closure_def_with_capture() {
    crate::assert_ok!(
        [
            crate Foo {
                closure my_closure [cap0 : by_value u32] () -> u32 { trusted }
            }
        ]
    )
}

/// Test: construct a closure value and assign it to a local.
#[test]
fn test_closure_construction() {
    crate::assert_ok!(
        [
            crate Foo {
                closure my_closure [cap0 : by_value u32] () -> u32 { trusted }

                fn caller(v1: u32) -> u32 = minirust {
                    exists {
                        let v2: closure_ty(my_closure);

                        bb0: {
                            statements {
                                local(v2) = closure my_closure { load(local(v1)) } as closure_ty(my_closure);
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

/// Test: construct a closure and call it.
#[test]
fn test_closure_call() {
    crate::assert_ok!(
        [
            crate Foo {
                closure my_closure [cap0 : by_value u32] (v1: u32) -> u32 { trusted }

                fn caller(v1: u32) -> u32 = minirust {
                    exists {
                        let v2: closure_ty(my_closure);

                        bb0: {
                            statements {
                                local(v2) = closure my_closure { load(local(v1)) } as closure_ty(my_closure);
                            }
                            call load(local(v2)) (Copy(constant(42: u32))) -> local(_return) goto bb1;
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

/// Test: closure with no captures and no arguments.
#[test]
fn test_closure_no_captures_no_args() {
    crate::assert_ok!(
        [
            crate Foo {
                closure my_closure [] () -> u32 { trusted }

                fn caller() -> u32 = minirust {
                    exists {
                        let v1: closure_ty(my_closure);

                        bb0: {
                            statements {
                                local(v1) = closure my_closure { } as closure_ty(my_closure);
                            }
                            call load(local(v1)) () -> local(_return) goto bb1;
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

/// Test: calling a closure with wrong argument type should fail.
#[test]
fn test_closure_call_wrong_arg_type() {
    crate::assert_err!(
        [
            crate Foo {
                closure my_closure [] (v1: u32) -> u32 { trusted }

                fn caller() -> u32 = minirust {
                    exists {
                        let v1: closure_ty(my_closure);

                        bb0: {
                            statements {
                                local(v1) = closure my_closure { } as closure_ty(my_closure);
                            }
                            call load(local(v1)) (Copy(constant(true))) -> local(_return) goto bb1;
                        }

                        bb1: {
                            statements {}
                            return;
                        }
                    }
                };
            }
        ]

        [ /* TODO */ ]

        expect_test::expect![[r#"
            the rule "call" at (nll.rs) failed because
              pattern `RigidName::FnDef(fn_id)` did not match value `closure_def(my_closure)`"#]]
    )
}
