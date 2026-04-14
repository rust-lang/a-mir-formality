/// Tests for issue #209: ensuring functions return a value on all paths.
///
/// These tests verify that a-mir-formality matches rustc's behavior
/// for return validation. All error cases have been verified against
/// rustc to ensure we accept a correct subset of Rust.

/// A function that declares a return type of u32 but has an empty body
/// should be an error — no value is returned.
/// rustc: "implicitly returns `()` as its body has no tail or `return` expression"
#[test]
#[ignore = "needs return validation (#209)"]
fn empty_body_non_unit_return() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo() -> u32 {
                }
            }
        ]
        expect_test::expect![[r#"function may not return a value"#]]
    )
}

/// A function returning () with an empty body is fine — unit is implicit.
/// rustc: compiles with no error
#[test]
fn empty_body_unit_return() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo() -> () {
                }
            }
        ]
    )
}

/// If/else where both branches return — all paths return, so this is fine.
/// rustc: compiles with no error
#[test]
fn if_else_both_branches_return() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo(b: bool) -> u32 {
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

/// If/else where only one branch returns — the other path falls through
/// without returning, so this should be an error.
/// rustc: "expected `u32`, found `()`"
#[test]
#[ignore = "needs return validation (#209)"]
fn if_else_one_branch_returns() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo(b: bool) -> u32 {
                    if b {
                        return 1 _ u32;
                    } else {
                    }
                }
            }
        ]
        expect_test::expect![[r#"function may not return a value"#]]
    )
}

/// An infinite loop never terminates, so it never needs to return.
/// rustc: compiles with no error (loop {} has type !)
#[test]
fn infinite_loop_no_return_needed() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo() -> u32 {
                    loop {
                    }
                }
            }
        ]
    )
}

/// A loop with break exits the loop, but then there's no return after it.
/// This should be an error.
/// rustc: "expected `u32`, found `()`"
#[test]
#[ignore = "needs return validation (#209)"]
fn loop_with_break_no_return() {
    crate::assert_err!(
        [
            crate Foo {
                fn foo() -> u32 {
                    'a: loop {
                        break 'a;
                    }
                }
            }
        ]
        expect_test::expect![[r#"function may not return a value"#]]
    )
}

/// A loop with break followed by a return is fine — all paths return.
/// rustc: compiles with no error
#[test]
fn loop_with_break_then_return() {
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

/// A simple function that returns a value on all paths.
/// rustc: compiles with no error
#[test]
fn simple_return() {
    crate::assert_ok!(
        [
            crate Foo {
                fn foo() -> u32 {
                    return 42 _ u32;
                }
            }
        ]
    )
}
