#![allow(non_snake_case)] // we embed type names into the names for our test functions

use a_mir_formality::{test_program_ok, test_where_clause};
use formality_core::test_util::ResultTestExt;
use formality_macros::test;

#[test]
fn test_const_syntax() {
    let gen_program = |addl: &str| {
        const BASE_PROGRAM: &str = "[
        crate core {
           const trait Default {
           }

           impl const Default for () {
           }
        }

        ]";

        BASE_PROGRAM.replace("ADDITIONAL", addl)
    };

    test_program_ok(&gen_program("")).assert_ok(expect_test::expect!["()"]);
}

#[test]
fn test_runtime_fn_with_runtime_effect() {
    let BASE_PROGRAM: &str = "[
        crate test {
            fn foo() -> () do runtime {(runtime)}
        }
    ]";
    test_where_clause(
        BASE_PROGRAM,
        "{} => {}",
    )
    .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }}"]);
}

#[test]
fn test_const_fn_with_runtime_effect() {
    let BASE_PROGRAM: &str = "[
        crate test {
            fn foo() -> () some_random_keyword const {(runtime)}
        }
    ]";
    test_where_clause(
        BASE_PROGRAM,
        "{} => {}",
    )
    .assert_err(expect_test::expect![[r#"
        judgment `prove { goal: {@ subset(runtime , const)}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
          failed at (src/file.rs:LL:CC) because
            judgment `prove_wc_list { goal: {@ subset(runtime , const)}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                judgment `prove_wc { goal: @ subset(runtime , const), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                  the rule "effect subset" failed at step #0 (src/file.rs:LL:CC) because
                    judgment `prove_effect_subset { subset: runtime, superset: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                      the rule "atomic" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_atomic_effect_subset { atomic_subeffect: runtime, superset: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                          the rule "transitive" failed at step #0 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_effect_upper_bound { f1: runtime, assumptions: {}, env: Env { variables: [], bias: Soundness } }`
                          the rule "union-subset-lhs" failed at step #1 (src/file.rs:LL:CC) because
                            judgment had no applicable rules: `prove_atomic_effect_eq { f1: runtime, f2: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }`

        Stack backtrace:
           0: anyhow::error::<impl core::convert::From<E> for anyhow::Error>::from
                     at /home/tiif/.cargo/registry/src/index.crates.io-6f17d22bba15001f/anyhow-1.0.75/src/backtrace.rs:27:14
           1: <core::result::Result<T,F> as core::ops::try_trait::FromResidual<core::result::Result<core::convert::Infallible,E>>>::from_residual
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/core/src/result.rs:1989:27
           2: formality_check::Check::prove_goal
                     at ./crates/formality-check/src/lib.rs:141:18
           3: formality_check::fns::<impl formality_check::Check>::check_fn
                     at ./crates/formality-check/src/fns.rs:69:21
           4: formality_check::fns::<impl formality_check::Check>::check_free_fn
                     at ./crates/formality-check/src/fns.rs:12:9
           5: formality_check::Check::check_crate_item
                     at ./crates/formality-check/src/lib.rs:116:33
           6: formality_check::Check::check_current_crate
                     at ./crates/formality-check/src/lib.rs:66:13
           7: formality_check::Check::check
                     at ./crates/formality-check/src/lib.rs:55:13
           8: formality_check::check_current_crate
                     at ./crates/formality-check/src/lib.rs:32:5
           9: formality_check::check_all_crates
                     at ./crates/formality-check/src/lib.rs:23:9
          10: a_mir_formality::test_where_clause::{{closure}}
                     at ./src/lib.rs:80:9
          11: tracing_core::dispatcher::with_default
                     at /home/tiif/.cargo/registry/src/index.crates.io-6f17d22bba15001f/tracing-core-0.1.32/src/dispatcher.rs:265:5
          12: tracing::subscriber::with_default
                     at /home/tiif/.cargo/registry/src/index.crates.io-6f17d22bba15001f/tracing-0.1.40/src/subscriber.rs:24:5
          13: formality_core::with_tracing_logs
                     at ./crates/formality-core/src/lib.rs:59:5
          14: a_mir_formality::test_where_clause
                     at ./src/lib.rs:78:5
          15: const_trait::test_const_fn_with_runtime_effect::{{closure}}
                     at ./tests/const_trait.rs:48:5
          16: tracing_core::dispatcher::with_default
                     at /home/tiif/.cargo/registry/src/index.crates.io-6f17d22bba15001f/tracing-core-0.1.32/src/dispatcher.rs:265:5
          17: tracing::subscriber::with_default
                     at /home/tiif/.cargo/registry/src/index.crates.io-6f17d22bba15001f/tracing-0.1.40/src/subscriber.rs:24:5
          18: formality_core::with_tracing_logs
                     at ./crates/formality-core/src/lib.rs:59:5
          19: const_trait::test_const_fn_with_runtime_effect
                     at ./tests/const_trait.rs:41:1
          20: const_trait::test_const_fn_with_runtime_effect::{{closure}}
                     at ./tests/const_trait.rs:42:39
          21: core::ops::function::FnOnce::call_once
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/core/src/ops/function.rs:250:5
          22: core::ops::function::FnOnce::call_once
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/core/src/ops/function.rs:250:5
          23: test::__rust_begin_short_backtrace
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/test/src/lib.rs:625:18
          24: test::run_test_in_process::{{closure}}
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/test/src/lib.rs:648:60
          25: <core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/core/src/panic/unwind_safe.rs:272:9
          26: std::panicking::try::do_call
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/std/src/panicking.rs:553:40
          27: std::panicking::try
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/std/src/panicking.rs:517:19
          28: std::panic::catch_unwind
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/std/src/panic.rs:350:14
          29: test::run_test_in_process
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/test/src/lib.rs:648:27
          30: test::run_test::{{closure}}
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/test/src/lib.rs:569:43
          31: test::run_test::{{closure}}
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/test/src/lib.rs:599:41
          32: std::sys_common::backtrace::__rust_begin_short_backtrace
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/std/src/sys_common/backtrace.rs:155:18
          33: std::thread::Builder::spawn_unchecked_::{{closure}}::{{closure}}
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/std/src/thread/mod.rs:542:17
          34: <core::panic::unwind_safe::AssertUnwindSafe<F> as core::ops::function::FnOnce<()>>::call_once
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/core/src/panic/unwind_safe.rs:272:9
          35: std::panicking::try::do_call
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/std/src/panicking.rs:553:40
          36: std::panicking::try
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/std/src/panicking.rs:517:19
          37: std::panic::catch_unwind
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/std/src/panic.rs:350:14
          38: std::thread::Builder::spawn_unchecked_::{{closure}}
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/std/src/thread/mod.rs:541:30
          39: core::ops::function::FnOnce::call_once{{vtable.shim}}
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/core/src/ops/function.rs:250:5
          40: <alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/alloc/src/boxed.rs:2062:9
          41: <alloc::boxed::Box<F,A> as core::ops::function::FnOnce<Args>>::call_once
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/alloc/src/boxed.rs:2062:9
          42: std::sys::pal::unix::thread::Thread::new::thread_start
                     at /rustc/3cf924b934322fd7b514600a7dc84fc517515346/library/std/src/sys/pal/unix/thread.rs:108:17
          43: start_thread
                     at ./nptl/pthread_create.c:442:8
          44: __GI___clone3
                     at ./misc/../sysdeps/unix/sysv/linux/x86_64/clone3.S:81"#]]);
}
