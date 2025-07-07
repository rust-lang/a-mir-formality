

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
                        v1 = v0;
                    }
                    return;
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
                        v0;
                    }
                    return;
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
                    }
                    goto bb1;

                    bb1: {
                        v1 = v0;
                    }
                    return;
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
                fn foo () -> () = minirust() -> v0 {
                    let v0: ();
                    let v1: ();
                    
                    bb0: {
                    }
                    call bar() -> local(v1) goto bb1;

                    bb1: {}
                    return;
                };

                fn bar() -> () = minirust() -> v0 {
                    let v0: ();
                    
                    bb0: {
                    }
                    return;
                };
            }
        ]
        expect_test::expect![["()"]]
    )

}
