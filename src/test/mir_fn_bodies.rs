
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
                          param2 = ()

            Stack backtrace:
               0: anyhow::error::<impl core::convert::From<E> for anyhow::Error>::from
                         at /home/tiif/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/anyhow-1.0.75/src/backtrace.rs:27:14
               1: <core::result::Result<T,F> as core::ops::try_trait::FromResidual<core::result::Result<core::convert::Infallible,E>>>::from_residual
                         at /home/tiif/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/result.rs:2079:27
               2: formality_check::Check::prove_goal
                         at ./crates/formality-check/src/lib.rs:150:18
               3: formality_check::mini_rust_check::<impl formality_check::Check>::check_statement
                         at ./crates/formality-check/src/mini_rust_check.rs:99:22
               4: formality_check::mini_rust_check::<impl formality_check::Check>::check_block
                         at ./crates/formality-check/src/mini_rust_check.rs:78:18
               5: formality_check::mini_rust_check::<impl formality_check::Check>::check_body
                         at ./crates/formality-check/src/mini_rust_check.rs:65:18
               6: formality_check::fns::<impl formality_check::Check>::check_fn
                         at ./crates/formality-check/src/fns.rs:76:26
               7: formality_check::fns::<impl formality_check::Check>::check_free_fn
                         at ./crates/formality-check/src/fns.rs:14:14
               8: formality_check::Check::check_crate_item
                         at ./crates/formality-check/src/lib.rs:125:38
               9: formality_check::Check::check_current_crate
                         at ./crates/formality-check/src/lib.rs:75:18
              10: formality_check::Check::check
                         at ./crates/formality-check/src/lib.rs:57:18
              11: formality_check::check_current_crate
                         at ./crates/formality-check/src/lib.rs:38:6
              12: formality_check::check_all_crates
                         at ./crates/formality-check/src/lib.rs:25:9
              13: a_mir_formality::test_program_ok
                         at ./src/lib.rs:64:5
              14: a_mir_formality::test::mir_fn_bodies::test_invalid_assign_statement
                         at ./src/lib.rs:58:9
              15: a_mir_formality::test::mir_fn_bodies::test_invalid_assign_statement::{{closure}}
                         at ./src/test/mir_fn_bodies.rs:123:35
              16: core::ops::function::FnOnce::call_once
                         at /home/tiif/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/ops/function.rs:250:5
              17: core::ops::function::FnOnce::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/ops/function.rs:250:5
              18: test::__rust_begin_short_backtrace
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:648:18
              19: test::run_test_in_process::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:671:74
              20: <core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/panic/unwind_safe.rs:272:9
              21: std::panicking::catch_unwind::do_call
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:589:40
              22: std::panicking::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:552:19
              23: std::panic::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panic.rs:359:14
              24: test::run_test_in_process
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:671:27
              25: test::run_test::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:592:43
              26: test::run_test::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:622:41
              27: std::sys::backtrace::__rust_begin_short_backtrace
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/sys/backtrace.rs:152:18
              28: std::thread::Builder::spawn_unchecked_::{{closure}}::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/thread/mod.rs:559:17
              29: <core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/panic/unwind_safe.rs:272:9
              30: std::panicking::catch_unwind::do_call
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:589:40
              31: std::panicking::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:552:19
              32: std::panic::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panic.rs:359:14
              33: std::thread::Builder::spawn_unchecked_::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/thread/mod.rs:557:30
              34: core::ops::function::FnOnce::call_once{{vtable.shim}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/ops/function.rs:250:5
              35: <alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/alloc/src/boxed.rs:1966:9
              36: <alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/alloc/src/boxed.rs:1966:9
              37: std::sys::pal::unix::thread::Thread::new::thread_start
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/sys/pal/unix/thread.rs:97:17
              38: start_thread
                         at ./nptl/pthread_create.c:442:8
              39: __GI___clone3
                         at ./misc/../sysdeps/unix/sysv/linux/x86_64/clone3.S:81:0"#]]
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
            PlaceExpression::Local: unknown local name

            Stack backtrace:
               0: anyhow::error::<impl anyhow::Error>::msg
                         at /home/tiif/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/anyhow-1.0.75/src/backtrace.rs:27:14
               1: anyhow::__private::format_err
                         at /home/tiif/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/anyhow-1.0.75/src/lib.rs:675:13
               2: formality_check::mini_rust_check::<impl formality_check::Check>::check_place
                         at ./crates/formality-check/src/mini_rust_check.rs:188:21
               3: formality_check::mini_rust_check::<impl formality_check::Check>::check_statement
                         at ./crates/formality-check/src/mini_rust_check.rs:111:22
               4: formality_check::mini_rust_check::<impl formality_check::Check>::check_block
                         at ./crates/formality-check/src/mini_rust_check.rs:78:18
               5: formality_check::mini_rust_check::<impl formality_check::Check>::check_body
                         at ./crates/formality-check/src/mini_rust_check.rs:65:18
               6: formality_check::fns::<impl formality_check::Check>::check_fn
                         at ./crates/formality-check/src/fns.rs:76:26
               7: formality_check::fns::<impl formality_check::Check>::check_free_fn
                         at ./crates/formality-check/src/fns.rs:14:14
               8: formality_check::Check::check_crate_item
                         at ./crates/formality-check/src/lib.rs:125:38
               9: formality_check::Check::check_current_crate
                         at ./crates/formality-check/src/lib.rs:75:18
              10: formality_check::Check::check
                         at ./crates/formality-check/src/lib.rs:57:18
              11: formality_check::check_current_crate
                         at ./crates/formality-check/src/lib.rs:38:6
              12: formality_check::check_all_crates
                         at ./crates/formality-check/src/lib.rs:25:9
              13: a_mir_formality::test_program_ok
                         at ./src/lib.rs:64:5
              14: a_mir_formality::test::mir_fn_bodies::test_invalid_local_in_place_mention
                         at ./src/lib.rs:58:9
              15: a_mir_formality::test::mir_fn_bodies::test_invalid_local_in_place_mention::{{closure}}
                         at ./src/test/mir_fn_bodies.rs:239:41
              16: core::ops::function::FnOnce::call_once
                         at /home/tiif/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/ops/function.rs:250:5
              17: core::ops::function::FnOnce::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/ops/function.rs:250:5
              18: test::__rust_begin_short_backtrace
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:648:18
              19: test::run_test_in_process::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:671:74
              20: <core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/panic/unwind_safe.rs:272:9
              21: std::panicking::catch_unwind::do_call
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:589:40
              22: std::panicking::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:552:19
              23: std::panic::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panic.rs:359:14
              24: test::run_test_in_process
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:671:27
              25: test::run_test::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:592:43
              26: test::run_test::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:622:41
              27: std::sys::backtrace::__rust_begin_short_backtrace
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/sys/backtrace.rs:152:18
              28: std::thread::Builder::spawn_unchecked_::{{closure}}::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/thread/mod.rs:559:17
              29: <core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/panic/unwind_safe.rs:272:9
              30: std::panicking::catch_unwind::do_call
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:589:40
              31: std::panicking::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:552:19
              32: std::panic::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panic.rs:359:14
              33: std::thread::Builder::spawn_unchecked_::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/thread/mod.rs:557:30
              34: core::ops::function::FnOnce::call_once{{vtable.shim}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/ops/function.rs:250:5
              35: <alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/alloc/src/boxed.rs:1966:9
              36: <alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/alloc/src/boxed.rs:1966:9
              37: std::sys::pal::unix::thread::Thread::new::thread_start
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/sys/pal/unix/thread.rs:97:17
              38: start_thread
                         at ./nptl/pthread_create.c:442:8
              39: __GI___clone3
                         at ./misc/../sysdeps/unix/sysv/linux/x86_64/clone3.S:81:0"#]]
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
            Basic block bb1 does not exist

            Stack backtrace:
               0: anyhow::error::<impl anyhow::Error>::msg
                         at /home/tiif/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/anyhow-1.0.75/src/backtrace.rs:27:14
               1: formality_check::mini_rust_check::<impl formality_check::Check>::check_block_exist
                         at ./crates/formality-check/src/mini_rust_check.rs:250:9
               2: formality_check::mini_rust_check::<impl formality_check::Check>::check_terminator
                         at ./crates/formality-check/src/mini_rust_check.rs:127:22
               3: formality_check::mini_rust_check::<impl formality_check::Check>::check_block
                         at ./crates/formality-check/src/mini_rust_check.rs:81:14
               4: formality_check::mini_rust_check::<impl formality_check::Check>::check_body
                         at ./crates/formality-check/src/mini_rust_check.rs:65:18
               5: formality_check::fns::<impl formality_check::Check>::check_fn
                         at ./crates/formality-check/src/fns.rs:76:26
               6: formality_check::fns::<impl formality_check::Check>::check_free_fn
                         at ./crates/formality-check/src/fns.rs:14:14
               7: formality_check::Check::check_crate_item
                         at ./crates/formality-check/src/lib.rs:125:38
               8: formality_check::Check::check_current_crate
                         at ./crates/formality-check/src/lib.rs:75:18
               9: formality_check::Check::check
                         at ./crates/formality-check/src/lib.rs:57:18
              10: formality_check::check_current_crate
                         at ./crates/formality-check/src/lib.rs:38:6
              11: formality_check::check_all_crates
                         at ./crates/formality-check/src/lib.rs:25:9
              12: a_mir_formality::test_program_ok
                         at ./src/lib.rs:64:5
              13: a_mir_formality::test::mir_fn_bodies::test_invalid_goto_bbid
                         at ./src/lib.rs:58:9
              14: a_mir_formality::test::mir_fn_bodies::test_invalid_goto_bbid::{{closure}}
                         at ./src/test/mir_fn_bodies.rs:348:28
              15: core::ops::function::FnOnce::call_once
                         at /home/tiif/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/ops/function.rs:250:5
              16: core::ops::function::FnOnce::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/ops/function.rs:250:5
              17: test::__rust_begin_short_backtrace
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:648:18
              18: test::run_test_in_process::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:671:74
              19: <core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/panic/unwind_safe.rs:272:9
              20: std::panicking::catch_unwind::do_call
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:589:40
              21: std::panicking::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:552:19
              22: std::panic::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panic.rs:359:14
              23: test::run_test_in_process
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:671:27
              24: test::run_test::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:592:43
              25: test::run_test::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:622:41
              26: std::sys::backtrace::__rust_begin_short_backtrace
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/sys/backtrace.rs:152:18
              27: std::thread::Builder::spawn_unchecked_::{{closure}}::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/thread/mod.rs:559:17
              28: <core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/panic/unwind_safe.rs:272:9
              29: std::panicking::catch_unwind::do_call
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:589:40
              30: std::panicking::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:552:19
              31: std::panic::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panic.rs:359:14
              32: std::thread::Builder::spawn_unchecked_::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/thread/mod.rs:557:30
              33: core::ops::function::FnOnce::call_once{{vtable.shim}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/ops/function.rs:250:5
              34: <alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/alloc/src/boxed.rs:1966:9
              35: <alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/alloc/src/boxed.rs:1966:9
              36: std::sys::pal::unix::thread::Thread::new::thread_start
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/sys/pal/unix/thread.rs:97:17
              37: start_thread
                         at ./nptl/pthread_create.c:442:8
              38: __GI___clone3
                         at ./misc/../sysdeps/unix/sysv/linux/x86_64/clone3.S:81:0"#]]
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
            The function called is not declared in current crate

            Stack backtrace:
               0: anyhow::error::<impl anyhow::Error>::msg
                         at /home/tiif/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/anyhow-1.0.75/src/backtrace.rs:27:14
               1: anyhow::__private::format_err
                         at /home/tiif/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/anyhow-1.0.75/src/lib.rs:675:13
               2: formality_check::mini_rust_check::<impl formality_check::Check>::check_value
                         at ./crates/formality-check/src/mini_rust_check.rs:219:21
               3: formality_check::mini_rust_check::<impl formality_check::Check>::check_terminator
                         at ./crates/formality-check/src/mini_rust_check.rs:137:22
               4: formality_check::mini_rust_check::<impl formality_check::Check>::check_block
                         at ./crates/formality-check/src/mini_rust_check.rs:81:14
               5: formality_check::mini_rust_check::<impl formality_check::Check>::check_body
                         at ./crates/formality-check/src/mini_rust_check.rs:65:18
               6: formality_check::fns::<impl formality_check::Check>::check_fn
                         at ./crates/formality-check/src/fns.rs:76:26
               7: formality_check::fns::<impl formality_check::Check>::check_free_fn
                         at ./crates/formality-check/src/fns.rs:14:14
               8: formality_check::Check::check_crate_item
                         at ./crates/formality-check/src/lib.rs:125:38
               9: formality_check::Check::check_current_crate
                         at ./crates/formality-check/src/lib.rs:75:18
              10: formality_check::Check::check
                         at ./crates/formality-check/src/lib.rs:57:18
              11: formality_check::check_current_crate
                         at ./crates/formality-check/src/lib.rs:38:6
              12: formality_check::check_all_crates
                         at ./crates/formality-check/src/lib.rs:25:9
              13: a_mir_formality::test_program_ok
                         at ./src/lib.rs:64:5
              14: a_mir_formality::test::mir_fn_bodies::test_call_invalid_fn
                         at ./src/lib.rs:58:9
              15: a_mir_formality::test::mir_fn_bodies::test_call_invalid_fn::{{closure}}
                         at ./src/test/mir_fn_bodies.rs:453:26
              16: core::ops::function::FnOnce::call_once
                         at /home/tiif/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/ops/function.rs:250:5
              17: core::ops::function::FnOnce::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/ops/function.rs:250:5
              18: test::__rust_begin_short_backtrace
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:648:18
              19: test::run_test_in_process::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:671:74
              20: <core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/panic/unwind_safe.rs:272:9
              21: std::panicking::catch_unwind::do_call
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:589:40
              22: std::panicking::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:552:19
              23: std::panic::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panic.rs:359:14
              24: test::run_test_in_process
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:671:27
              25: test::run_test::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:592:43
              26: test::run_test::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:622:41
              27: std::sys::backtrace::__rust_begin_short_backtrace
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/sys/backtrace.rs:152:18
              28: std::thread::Builder::spawn_unchecked_::{{closure}}::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/thread/mod.rs:559:17
              29: <core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/panic/unwind_safe.rs:272:9
              30: std::panicking::catch_unwind::do_call
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:589:40
              31: std::panicking::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:552:19
              32: std::panic::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panic.rs:359:14
              33: std::thread::Builder::spawn_unchecked_::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/thread/mod.rs:557:30
              34: core::ops::function::FnOnce::call_once{{vtable.shim}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/ops/function.rs:250:5
              35: <alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/alloc/src/boxed.rs:1966:9
              36: <alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/alloc/src/boxed.rs:1966:9
              37: std::sys::pal::unix::thread::Thread::new::thread_start
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/sys/pal/unix/thread.rs:97:17
              38: start_thread
                         at ./nptl/pthread_create.c:442:8
              39: __GI___clone3
                         at ./misc/../sysdeps/unix/sysv/linux/x86_64/clone3.S:81:0"#]]
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
        expect_test::expect!["()"]
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
            There should be next block for Terminator::Call, but it does not exist!

            Stack backtrace:
               0: anyhow::error::<impl anyhow::Error>::msg
                         at /home/tiif/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/anyhow-1.0.75/src/backtrace.rs:27:14
               1: anyhow::__private::format_err
                         at /home/tiif/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/anyhow-1.0.75/src/lib.rs:675:13
               2: formality_check::mini_rust_check::<impl formality_check::Check>::check_terminator
                         at ./crates/formality-check/src/mini_rust_check.rs:170:21
               3: formality_check::mini_rust_check::<impl formality_check::Check>::check_block
                         at ./crates/formality-check/src/mini_rust_check.rs:84:14
               4: formality_check::mini_rust_check::<impl formality_check::Check>::check_body
                         at ./crates/formality-check/src/mini_rust_check.rs:68:18
               5: formality_check::fns::<impl formality_check::Check>::check_fn
                         at ./crates/formality-check/src/fns.rs:76:26
               6: formality_check::fns::<impl formality_check::Check>::check_free_fn
                         at ./crates/formality-check/src/fns.rs:14:14
               7: formality_check::Check::check_crate_item
                         at ./crates/formality-check/src/lib.rs:125:38
               8: formality_check::Check::check_current_crate
                         at ./crates/formality-check/src/lib.rs:75:18
               9: formality_check::Check::check
                         at ./crates/formality-check/src/lib.rs:57:18
              10: formality_check::check_current_crate
                         at ./crates/formality-check/src/lib.rs:38:6
              11: formality_check::check_all_crates
                         at ./crates/formality-check/src/lib.rs:25:9
              12: a_mir_formality::test_program_ok
                         at ./src/lib.rs:64:5
              13: a_mir_formality::test::mir_fn_bodies::test_no_next_bb_for_call_terminator
                         at ./src/lib.rs:58:9
              14: a_mir_formality::test::mir_fn_bodies::test_no_next_bb_for_call_terminator::{{closure}}
                         at ./src/test/mir_fn_bodies.rs:604:41
              15: core::ops::function::FnOnce::call_once
                         at /home/tiif/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/ops/function.rs:250:5
              16: core::ops::function::FnOnce::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/ops/function.rs:250:5
              17: test::__rust_begin_short_backtrace
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:648:18
              18: test::run_test_in_process::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:671:74
              19: <core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/panic/unwind_safe.rs:272:9
              20: std::panicking::catch_unwind::do_call
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:589:40
              21: std::panicking::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:552:19
              22: std::panic::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panic.rs:359:14
              23: test::run_test_in_process
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:671:27
              24: test::run_test::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:592:43
              25: test::run_test::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:622:41
              26: std::sys::backtrace::__rust_begin_short_backtrace
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/sys/backtrace.rs:152:18
              27: std::thread::Builder::spawn_unchecked_::{{closure}}::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/thread/mod.rs:559:17
              28: <core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/panic/unwind_safe.rs:272:9
              29: std::panicking::catch_unwind::do_call
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:589:40
              30: std::panicking::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:552:19
              31: std::panic::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panic.rs:359:14
              32: std::thread::Builder::spawn_unchecked_::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/thread/mod.rs:557:30
              33: core::ops::function::FnOnce::call_once{{vtable.shim}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/ops/function.rs:250:5
              34: <alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/alloc/src/boxed.rs:1966:9
              35: <alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/alloc/src/boxed.rs:1966:9
              36: std::sys::pal::unix::thread::Thread::new::thread_start
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/sys/pal/unix/thread.rs:97:17
              37: start_thread
                         at ./nptl/pthread_create.c:442:8
              38: __GI___clone3
                         at ./misc/../sysdeps/unix/sysv/linux/x86_64/clone3.S:81:0"#]]
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
                          param2 = ()

            Stack backtrace:
               0: anyhow::error::<impl core::convert::From<E> for anyhow::Error>::from
                         at /home/tiif/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/anyhow-1.0.75/src/backtrace.rs:27:14
               1: <core::result::Result<T,F> as core::ops::try_trait::FromResidual<core::result::Result<core::convert::Infallible,E>>>::from_residual
                         at /home/tiif/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/result.rs:2079:27
               2: formality_check::Check::prove_goal
                         at ./crates/formality-check/src/lib.rs:150:18
               3: formality_check::mini_rust_check::<impl formality_check::Check>::check_body
                         at ./crates/formality-check/src/mini_rust_check.rs:46:14
               4: formality_check::fns::<impl formality_check::Check>::check_fn
                         at ./crates/formality-check/src/fns.rs:76:26
               5: formality_check::fns::<impl formality_check::Check>::check_free_fn
                         at ./crates/formality-check/src/fns.rs:14:14
               6: formality_check::Check::check_crate_item
                         at ./crates/formality-check/src/lib.rs:125:38
               7: formality_check::Check::check_current_crate
                         at ./crates/formality-check/src/lib.rs:75:18
               8: formality_check::Check::check
                         at ./crates/formality-check/src/lib.rs:57:18
               9: formality_check::check_current_crate
                         at ./crates/formality-check/src/lib.rs:38:6
              10: formality_check::check_all_crates
                         at ./crates/formality-check/src/lib.rs:25:9
              11: a_mir_formality::test_program_ok
                         at ./src/lib.rs:64:5
              12: a_mir_formality::test::mir_fn_bodies::test_uncompatible_return_type
                         at ./src/lib.rs:58:9
              13: a_mir_formality::test::mir_fn_bodies::test_uncompatible_return_type::{{closure}}
                         at ./src/test/mir_fn_bodies.rs:135:35
              14: core::ops::function::FnOnce::call_once
                         at /home/tiif/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/ops/function.rs:250:5
              15: core::ops::function::FnOnce::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/ops/function.rs:250:5
              16: test::__rust_begin_short_backtrace
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:648:18
              17: test::run_test_in_process::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:671:74
              18: <core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/panic/unwind_safe.rs:272:9
              19: std::panicking::catch_unwind::do_call
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:589:40
              20: std::panicking::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:552:19
              21: std::panic::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panic.rs:359:14
              22: test::run_test_in_process
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:671:27
              23: test::run_test::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:592:43
              24: test::run_test::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:622:41
              25: std::sys::backtrace::__rust_begin_short_backtrace
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/sys/backtrace.rs:152:18
              26: std::thread::Builder::spawn_unchecked_::{{closure}}::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/thread/mod.rs:559:17
              27: <core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/panic/unwind_safe.rs:272:9
              28: std::panicking::catch_unwind::do_call
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:589:40
              29: std::panicking::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:552:19
              30: std::panic::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panic.rs:359:14
              31: std::thread::Builder::spawn_unchecked_::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/thread/mod.rs:557:30
              32: core::ops::function::FnOnce::call_once{{vtable.shim}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/ops/function.rs:250:5
              33: <alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/alloc/src/boxed.rs:1966:9
              34: <alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/alloc/src/boxed.rs:1966:9
              35: std::sys::pal::unix::thread::Thread::new::thread_start
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/sys/pal/unix/thread.rs:97:17
              36: start_thread
                         at ./nptl/pthread_create.c:442:8
              37: __GI___clone3
                         at ./misc/../sysdeps/unix/sysv/linux/x86_64/clone3.S:81:0"#]]
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
            The return local variable has not been initialized.

            Stack backtrace:
               0: anyhow::error::<impl anyhow::Error>::msg
                         at /home/tiif/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/anyhow-1.0.75/src/backtrace.rs:27:14
               1: anyhow::__private::format_err
                         at /home/tiif/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/anyhow-1.0.75/src/lib.rs:675:13
               2: formality_check::mini_rust_check::<impl formality_check::Check>::check_terminator
                         at ./crates/formality-check/src/mini_rust_check.rs:170:21
               3: formality_check::mini_rust_check::<impl formality_check::Check>::check_block
                         at ./crates/formality-check/src/mini_rust_check.rs:81:14
               4: formality_check::mini_rust_check::<impl formality_check::Check>::check_body
                         at ./crates/formality-check/src/mini_rust_check.rs:65:18
               5: formality_check::fns::<impl formality_check::Check>::check_fn
                         at ./crates/formality-check/src/fns.rs:76:26
               6: formality_check::fns::<impl formality_check::Check>::check_free_fn
                         at ./crates/formality-check/src/fns.rs:14:14
               7: formality_check::Check::check_crate_item
                         at ./crates/formality-check/src/lib.rs:125:38
               8: formality_check::Check::check_current_crate
                         at ./crates/formality-check/src/lib.rs:75:18
               9: formality_check::Check::check
                         at ./crates/formality-check/src/lib.rs:57:18
              10: formality_check::check_current_crate
                         at ./crates/formality-check/src/lib.rs:38:6
              11: formality_check::check_all_crates
                         at ./crates/formality-check/src/lib.rs:25:9
              12: a_mir_formality::test_program_ok
                         at ./src/lib.rs:64:5
              13: a_mir_formality::test::mir_fn_bodies::test_uninitialised_return_type
                         at ./src/lib.rs:58:9
              14: a_mir_formality::test::mir_fn_bodies::test_uninitialised_return_type::{{closure}}
                         at ./src/test/mir_fn_bodies.rs:137:36
              15: core::ops::function::FnOnce::call_once
                         at /home/tiif/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/core/src/ops/function.rs:250:5
              16: core::ops::function::FnOnce::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/ops/function.rs:250:5
              17: test::__rust_begin_short_backtrace
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:648:18
              18: test::run_test_in_process::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:671:74
              19: <core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/panic/unwind_safe.rs:272:9
              20: std::panicking::catch_unwind::do_call
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:589:40
              21: std::panicking::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:552:19
              22: std::panic::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panic.rs:359:14
              23: test::run_test_in_process
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:671:27
              24: test::run_test::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:592:43
              25: test::run_test::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/test/src/lib.rs:622:41
              26: std::sys::backtrace::__rust_begin_short_backtrace
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/sys/backtrace.rs:152:18
              27: std::thread::Builder::spawn_unchecked_::{{closure}}::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/thread/mod.rs:559:17
              28: <core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/panic/unwind_safe.rs:272:9
              29: std::panicking::catch_unwind::do_call
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:589:40
              30: std::panicking::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panicking.rs:552:19
              31: std::panic::catch_unwind
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/panic.rs:359:14
              32: std::thread::Builder::spawn_unchecked_::{{closure}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/thread/mod.rs:557:30
              33: core::ops::function::FnOnce::call_once{{vtable.shim}}
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/core/src/ops/function.rs:250:5
              34: <alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/alloc/src/boxed.rs:1966:9
              35: <alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/alloc/src/boxed.rs:1966:9
              36: std::sys::pal::unix::thread::Thread::new::thread_start
                         at /rustc/71e4c005caa812a16fcb08d0bf1e6f1eda7c8381/library/std/src/sys/pal/unix/thread.rs:97:17
              37: start_thread
                         at ./nptl/pthread_create.c:442:8
              38: __GI___clone3
                         at ./misc/../sysdeps/unix/sysv/linux/x86_64/clone3.S:81:0"#]]
    )
}