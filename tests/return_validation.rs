use a_mir_formality::{crates, FormalityTest};

/// Tests for issues #209 and #392: ensuring functions return a value on all paths.
///
/// These tests verify that a-mir-formality matches rustc's behavior
/// for return validation. All error cases have been verified against
/// rustc to ensure we accept a correct subset of Rust.

/// A function that declares a return type of u32 but has an empty body
/// should be an error — no value is returned.
/// rustc: "implicitly returns `()` as its body has no tail or `return` expression"
#[test]
fn empty_body_non_unit_return() {
    FormalityTest::new(crates![crate Foo {
        fn foo() -> u32 {
        }
    }])
    .err(expect_test::expect![[r#"
        the rule "non-unit return" at (return_check.rs) failed because
          function may not return a value

        the rule "unit" at (return_check.rs) failed because
          condition evaluated to false: `output_ty == &Ty::unit()`"#]])
}

/// A function returning () with an empty body is fine — unit is implicit.
/// rustc: compiles with no error
#[test]
fn empty_body_unit_return() {
    FormalityTest::new(crates![crate Foo {
        fn foo() -> () {
        }
    }])
    .skip_execute()
    .rustc_ok()
    .ok()
}

/// If/else where both branches return — all paths return, so this is fine.
/// rustc: compiles with no error
#[test]
fn if_else_both_branches_return() {
    FormalityTest::new(crates![crate Foo {
        fn foo(b: bool) -> u32 {
            if b {
                return 1_u32;
            } else {
                return 2_u32;
            }
        }
    }])
    .skip_execute()
    .rustc_ok()
    .ok()
}

/// If/else where only one branch returns — the other path falls through
/// without returning, so this should be an error.
/// rustc: "expected `u32`, found `()`"
#[test]
fn if_else_one_branch_returns() {
    FormalityTest::new(crates![crate Foo {
        fn foo(b: bool) -> u32 {
            if b {
                return 1_u32;
            } else {
            }
        }
    }])
    .err(expect_test::expect![[r#"
        the rule "non-unit return" at (return_check.rs) failed because
          function may not return a value

        the rule "unit" at (return_check.rs) failed because
          condition evaluated to false: `output_ty == &Ty::unit()`"#]])
}

/// An infinite loop never terminates, so it never needs to return.
/// rustc: compiles with no error (loop {} has type !)
#[test]
fn infinite_loop_no_return_needed() {
    FormalityTest::new(crates![crate Foo {
        fn foo() -> u32 {
            loop {
            }
        }
    }])
    .skip_execute()
    .rustc_ok()
    .ok()
}

/// A loop with break exits the loop, but then there's no return after it.
/// This should be an error.
/// rustc: "expected `u32`, found `()`"
#[test]
fn loop_with_break_no_return() {
    FormalityTest::new(crates![crate Foo {
        fn foo() -> u32 {
            'a: loop {
                break 'a;
            }
        }
    }])
    .err(expect_test::expect![[r#"
        the rule "non-unit return" at (return_check.rs) failed because
          function may not return a value

        the rule "unit" at (return_check.rs) failed because
          condition evaluated to false: `output_ty == &Ty::unit()`"#]])
}

/// A loop with break followed by a return is fine — all paths return.
/// rustc: compiles with no error
#[test]
fn loop_with_break_then_return() {
    FormalityTest::new(crates![crate Foo {
        fn foo() -> u32 {
            'a: loop {
                break 'a;
            }
            return 0_u32;
        }
    }])
    .skip_execute()
    .rustc_ok()
    .ok()
}

/// A simple function that returns a value on all paths.
/// rustc: compiles with no error
#[test]
fn simple_return() {
    FormalityTest::new(crates![crate Foo {
        fn foo() -> u32 {
            return 42_u32;
        }
    }])
    .skip_execute()
    .rustc_ok()
    .ok()
}

/// A break in dead code must not make the loop exit reachable.
#[test]
fn unreachable_break_after_return_does_not_revive_loop_exit() {
    let program = crates![crate Foo {
        fn foo() -> u32 {
            'a: loop {
                return 1_u32;
                break 'a;
            }
        }
    }];

    // The dead break contributes no reachable exit in Formality. In generated Rust
    // it still supplies `()` to a tail loop whose expected type is `u32`.
    let proof = a_mir_formality::test_program_ok(program)
        .expect("the dead break cannot reach function exit");
    formality_core::judgment::coverage::record_coverage(std::iter::once(&proof));
    // FIXME(#558): Reconcile the generated tail loop's type with Formality,
    // then update the rustc expectation here.
    let (ok, stderr) = formality_rust::to_rust::test_util::run_rustc(program);
    assert!(
        !ok && stderr.contains("E0308") && stderr.contains("expected `u32`, found `()`"),
        "{stderr}"
    );
}

/// A reachable break on one branch means the loop can fall through.
#[test]
fn loop_with_conditional_break_can_fall_through() {
    FormalityTest::new(crates![crate Foo {
        fn foo(b: bool) -> u32 {
            'a: loop {
                if b {
                    return 1_u32;
                } else {
                    break 'a;
                }
            }
        }
    }])
    .err(expect_test::expect![[r#"
        the rule "non-unit return" at (return_check.rs) failed because
          function may not return a value

        the rule "unit" at (return_check.rs) failed because
          condition evaluated to false: `output_ty == &Ty::unit()`"#]])
}

#[test]
fn if_without_else_is_rejected_for_non_unit_function() {
    FormalityTest::new(crates![crate Foo {
      fn foo(a: bool) -> u32 {
        if a {
          return 1_u32;
        }
      }
    }])
    .err(expect_test::expect![[r#"
        the rule "non-unit return" at (return_check.rs) failed because
          function may not return a value

        the rule "unit" at (return_check.rs) failed because
          condition evaluated to false: `output_ty == &Ty::unit()`"#]])
}

#[test]
fn loop_with_matching_continue_does_not_fall_through() {
    FormalityTest::new(crates![crate Foo {
      fn foo(a: bool) -> u32 {
        'a: loop {
          continue 'a;
        }
      }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

/// A break targeting an outer block propagates through the inner loop
/// and makes execution continue after the outer block.
#[test]
fn break_targeting_outer_block_makes_fall_through() {
    FormalityTest::new(crates![crate Foo {
      fn foo() -> u32 {
        'outer: {
          'inner: loop {
            break 'outer;
          }
        }
      }
    }])
    .err(expect_test::expect![[r#"
        the rule "non-unit return" at (return_check.rs) failed because
          function may not return a value

        the rule "unit" at (return_check.rs) failed because
          condition evaluated to false: `output_ty == &Ty::unit()`"#]])
}

#[test]
fn exists_with_return_is_accepted() {
    FormalityTest::new(crates![crate Foo {
      fn foo() -> i32 {

       exists<'a> {
          return 1_i32;
       }
     }
    }])
    .skip_execute()
    .rustc_ok()
    .ok();
}

#[test]
fn break_before_return_reaches_end_of_function() {
    let program = crates![crate Foo {
        fn foo() -> u32 {
            'block: {
                break 'block;
                return 22_u32;
            }
        }
    }];
    let (ok, stderr) = formality_rust::to_rust::test_util::run_rustc(program);
    assert!(!ok && stderr.contains("E0308"), "{stderr}");
    FormalityTest::new(program).err(expect_test::expect![[r#"
        the rule "non-unit return" at (return_check.rs) failed because
          function may not return a value

        the rule "unit" at (return_check.rs) failed because
          condition evaluated to false: `output_ty == &Ty::unit()`"#]]);
}

#[test]
fn break_outer_skips_return_after_inner_loop() {
    let program = crates![crate Foo {
        fn foo(c: bool) -> u32 {
            'outer: loop {
                'inner: loop {
                    if c { break 'inner; } else { break 'outer; }
                }
                return 22_u32;
            }
        }
    }];
    let (ok, stderr) = formality_rust::to_rust::test_util::run_rustc(program);
    assert!(!ok && stderr.contains("E0308"), "{stderr}");
    FormalityTest::new(program).err(expect_test::expect![[r#"
        the rule "non-unit return" at (return_check.rs) failed because
          function may not return a value

        the rule "unit" at (return_check.rs) failed because
          condition evaluated to false: `output_ty == &Ty::unit()`"#]]);
}

#[test]
fn continue_outer_or_return_after_inner_loop() {
    FormalityTest::new(crates![crate Foo {
        fn foo(c: bool) -> u32 {
            'outer: loop {
                'inner: loop {
                    if c { break 'inner; } else { continue 'outer; }
                }
                return 22_u32;
            }
        }
    }])
    .rustc_ok()
    .skip_execute()
    .ok();
}

#[test]
fn loop_can_return_or_repeat() {
    FormalityTest::new(crates![crate Foo {
        fn condition() -> bool { return true; }
        fn foo() -> u32 {
            'again: loop { if condition() { return 22_u32; } }
        }
    }])
    .rustc_ok()
    .skip_execute()
    .ok();
}

#[test]
fn outer_break_skips_borrow_in_inner_block() {
    let program = crates![crate Foo {
        fn main() -> () {
            exists<'p, 'x, 'y> {
                let x: u32 = 22_u32;
                let y: u32 = 0_u32;
                let p: &'p u32 = &'x x;
                'outer: {
                    'inner: { break 'outer; }
                    p = &'y y;
                }
                y = 1_u32;
                println!(*p);
            }
        }
    }];
    let (ok, stderr) = formality_rust::to_rust::test_util::run_rustc(program);
    assert!(ok, "{stderr}");
    // FIXME(#209): The borrow checker carries the skipped loan of `y` past the block.
    FormalityTest::new(program).expect_output("22\n").ok();
}
