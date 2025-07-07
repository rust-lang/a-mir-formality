
// All the tests are still failing because the parser cannot accept any statement for some reason?
#[test]
fn test_assign_statement() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo (u32) -> u32 = minirust(v1) -> v0 {
                    let v0: u32;
                    let v1: u32;
                    
                    bb0: {
                        statement { 
                            local(v1) = v0;
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
                        statement {
                            local(v0);
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
                        statement {}
                        goto bb1;
                    }

                    bb1: {
                        statement {
                            local(v1) = v0;
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
                fn foo () -> () = minirust(v1) -> v0 {
                    let v0: ();
                    let v1: ();
                    
                    bb0: {
                        statement {
                            local(v0) = v1;
                        }
                        call bar() -> local(v1) goto bb1;
                    }

                    bb1: {
                        statement {}
                        return;
                    }
                    
                };

                fn bar() -> () = minirust(v1) -> v0 {
                    let v0: ();
                    let v1: ();
                    
                    bb0: {
                        statement {
                            local(v0) = v1;
                        } 
                        return;
                    }
                    
                };
            }
        ]
        expect_test::expect![["()"]]
    )

}
