
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

// TODO: we need to add a rule for () <: ()
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
                            placeexpr_local(v0) = load(placeexpr_local(v1));
                            placeexpr_local(v0);
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
