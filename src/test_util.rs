use expect_test::Expect;
use formality_core::{test_util::AnyhowResultTestExt, with_tracing_logs};
use formality_rust::grammar::FeatureGateName;

use crate::{run_rustc, test_program_ok, test_program_ok_with_feature_gates};

/// Stringify a list of crate declarations and wrap them in the `[ ... ]`
/// brackets that the Crates grammar expects.
#[macro_export]
macro_rules! crates {
    ($($t:tt)*) => { ::core::concat!("[", ::core::stringify!($($t)*), "]") };
}

enum BackendExpect {
    Ok,
    Err(Expect),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BorrowCheckFailure {
    All,
    Alpha,
    Nll,
}

impl BorrowCheckFailure {
    fn name(self) -> &'static str {
        match self {
            BorrowCheckFailure::Nll => "nll",
            BorrowCheckFailure::Alpha => "polonius-alpha",
            BorrowCheckFailure::All => "polonius-unlocked",
        }
    }

    fn describe_rejection(self) -> String {
        format!("rejected by {} and more restrictive modes", self.name(),)
    }

    fn feature_gates(self) -> Vec<FeatureGateName> {
        match self {
            BorrowCheckFailure::Nll => vec![],
            BorrowCheckFailure::Alpha => vec![FeatureGateName::PoloniusAlpha],
            BorrowCheckFailure::All => vec![FeatureGateName::PoloniusUnlocked],
        }
    }
}

/// Default maximum number of MiniRust `machine.step()` calls per execution.
/// Tests needing more steps can override this with `execution_step_limit`.
const DEFAULT_EXECUTION_STEP_LIMIT: usize = 10_000;

/// Builder for a test program and its per-backend expectations.
pub struct FormalityTest {
    input: String,
    rustc_override: Option<BackendExpect>,
    skip_execute: bool,
    expected_output: Option<String>,
    execution_step_limit: usize,
}

impl FormalityTest {
    pub fn new(input: impl Into<String>) -> Self {
        Self {
            input: input.into(),
            rustc_override: None,
            skip_execute: false,
            expected_output: None,
            execution_step_limit: DEFAULT_EXECUTION_STEP_LIMIT,
        }
    }

    /// Skip execution (codegen + run) even if the program type-checks.
    /// Use for programs that have no `main` or test features codegen doesn't support yet.
    pub fn skip_execute(mut self) -> Self {
        self.skip_execute = true;
        self
    }

    /// Assert that execution produces the given stdout output.
    pub fn expect_output(mut self, output: impl Into<String>) -> Self {
        self.expected_output = Some(output.into());
        self
    }

    /// Set the maximum number of MiniRust `machine.step()` calls for this test.
    ///
    /// The default is 10,000 steps. This applies to both `ok` and the borrow-check
    /// runner, and is ignored when execution is skipped. A zero limit permits no
    /// steps; finishing on the last allowed step succeeds. This bounds execution
    /// steps, not elapsed time or the work of parsing, checking, and codegen.
    pub fn execution_step_limit(mut self, limit: usize) -> Self {
        self.execution_step_limit = limit;
        self
    }

    /// Require rustc to accept the program (always runs rustc).
    pub fn rustc_ok(mut self) -> Self {
        self.rustc_override = Some(BackendExpect::Ok);
        self
    }

    /// Require rustc to reject the program with the given stderr (always runs rustc).
    pub fn rustc_err(mut self, expect: Expect) -> Self {
        self.rustc_override = Some(BackendExpect::Err(expect));
        self
    }

    /// Assert formality accepts the program. After type-checking passes,
    /// also runs codegen + execution unless `.skip_execute()` was called.
    /// Also runs rustc if overridden or if `FORMALITY_RUN_RUSTC=1`.
    #[track_caller]
    pub fn ok(self) {
        let Self {
            input,
            rustc_override,
            skip_execute,
            expected_output,
            execution_step_limit,
        } = self;
        let proof_tree =
            with_tracing_logs(|| test_program_ok(&input).expect("expected program to pass"));
        formality_core::judgment::coverage::record_coverage(std::iter::once(&proof_tree));

        if !skip_execute {
            execute_and_check_output(&input, expected_output.as_deref(), execution_step_limit);
        }

        let rustc_expect = match rustc_override {
            Some(expect) => Some(expect),
            None if run_rustc() => Some(BackendExpect::Ok),
            None => None,
        };
        check_rustc(&input, rustc_expect);
    }

    /// Assert formality rejects the program with the given error. Also runs
    /// rustc if overridden.
    #[track_caller]
    pub fn err(self, expect: Expect) {
        let Self {
            input,
            rustc_override,
            skip_execute: _,
            expected_output: _,
            execution_step_limit: _,
        } = self;

        test_program_ok(&input).assert_err_leaves(expect);
        check_rustc(&input, rustc_override);
    }

    /// Assert formality accepts the program under every borrowck mode. After
    /// type-checking passes, also runs codegen + execution unless
    /// `.skip_execute()` was called.
    /// Also runs rustc once, after every mode,
    /// if overridden (`FORMALITY_RUN_RUSTC` is not used here).
    #[track_caller]
    pub fn borrowck_ok(self) {
        if let Err((failure, error)) = self.borrowck_run() {
            panic!(
                "expected every mode to accept this program, but it was {}:\n{}",
                failure.describe_rejection(),
                error,
            );
        }
        check_rustc(&self.input, self.rustc_override);
    }

    /// Assert formality rejects the program with the given error for the passed
    /// `BorrowCheckFailure` mode and all more restrictive modes.
    /// Also runs rustc once, after every mode,
    /// if overridden (`FORMALITY_RUN_RUSTC` is not used here).
    #[track_caller]
    pub fn borrowck_err(self, expected: BorrowCheckFailure, expect: Expect) {
        match self.borrowck_run() {
            Ok(()) => panic!(
                "expected this program to be {}, but every mode accepted it",
                expected.describe_rejection(),
            ),
            Err((failure, error)) if failure != expected => panic!(
                "expected this program to be {}, but it was {}:\n{}",
                expected.describe_rejection(),
                failure.describe_rejection(),
                error,
            ),
            Err((_, output)) => {
                expect.assert_eq(&output);
            }
        }
        check_rustc(&self.input, self.rustc_override);
    }

    /// Run the program under every mode. `Ok(())` if every mode accepted it,
    /// otherwise the least permissive mode that was rejected and the error the
    /// rejecting modes reported. Asserts on the way that those modes agree on
    /// the error. And that more permissive modes don't reject a program that a
    /// more restrictive mode accepts.
    #[track_caller]
    fn borrowck_run(&self) -> Result<(), (BorrowCheckFailure, String)> {
        let mut error = Ok(());

        for mode in [
            BorrowCheckFailure::All,
            BorrowCheckFailure::Alpha,
            BorrowCheckFailure::Nll,
        ] {
            match test_program_ok_with_feature_gates(&self.input, mode.feature_gates()) {
                Ok(proof_tree) => {
                    formality_core::judgment::coverage::record_coverage(std::iter::once(
                        &proof_tree,
                    ));
                    if mode == BorrowCheckFailure::Nll && !self.skip_execute {
                        execute_and_check_output(
                            &self.input,
                            self.expected_output.as_deref(),
                            self.execution_step_limit,
                        );
                    }

                    if error.is_err() {
                        panic!("{mode:?} passed but a more permissive mode rejected it");
                    }
                }
                Err(e) => {
                    formality_core::test_util::record_negative_coverage_from_anyhow(&e);
                    let test_error = formality_core::test_util::normalize_paths(
                        formality_core::test_util::format_error_leaves(&e),
                    );
                    match &error {
                        Ok(()) => {
                            error = Err((mode, test_error.clone()));
                        }
                        Err((_, first)) => {
                            if first != &test_error {
                                panic!("{mode:?} does not emit the same error as a more permissive mode:\n{first}\n\n{test_error}");
                            }
                        }
                    }
                }
            }
        }

        error
    }
}
#[track_caller]
fn execute_and_check_output(input: &str, expected_output: Option<&str>, step_limit: usize) {
    let stdout = execute_program(input, step_limit);

    if let Some(expected) = expected_output {
        assert_eq!(stdout, expected, "program output mismatch");
    }
}

#[track_caller]
fn execute_program(input: &str, step_limit: usize) -> String {
    let crates: formality_rust::grammar::Crates =
        formality_rust::rust::try_term(input).expect("failed to parse program");

    let has_main = crates.crates.iter().any(|c| {
        c.items.iter().any(
            |item| matches!(item, formality_rust::grammar::CrateItem::Fn(f) if &**f.id == "main"),
        )
    });

    if !has_main {
        panic!("program has no `main` function — add one, or call `.skip_execute()` on the test");
    }

    let program = formality_rust::codegen::codegen_program(&crates).expect("codegen failed");

    let stdout_buf = std::sync::Arc::new(std::sync::Mutex::new(Vec::<u8>::new()));
    let stderr_buf = std::sync::Arc::new(std::sync::Mutex::new(Vec::<u8>::new()));

    let stdout = libspecr::DynWrite::new(SharedWriter(stdout_buf.clone()));
    let stderr = libspecr::DynWrite::new(SharedWriter(stderr_buf.clone()));

    type Mem = minirust_rs::mem::BasicMemory<minirust_rs::prelude::x86_64>;
    let mut machine: minirust_rs::lang::Machine<Mem> =
        minirust_rs::lang::Machine::new(program, (), stdout, stderr)
            .get_internal()
            .expect("machine creation failed");

    // Check before each call so a stop on the final permitted step still succeeds.
    let mut steps = 0;
    loop {
        if steps >= step_limit {
            panic!("execution step limit reached after {steps} steps; increase .execution_step_limit(n) if this program needs more steps");
        }

        steps += 1;
        match machine.step().get_internal() {
            Ok(()) => continue,
            Err(minirust_rs::prelude::TerminationInfo::MachineStop) => break,
            Err(e) => panic!("execution error: {e:?}"),
        }
    }
    let bytes = stdout_buf.lock().unwrap().clone();
    String::from_utf8(bytes).expect("stdout was not valid UTF-8")
}

struct SharedWriter(std::sync::Arc<std::sync::Mutex<Vec<u8>>>);

impl std::io::Write for SharedWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.lock().unwrap().extend_from_slice(buf);
        Ok(buf.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl libspecr::hidden::GcCompat for SharedWriter {
    fn points_to(&self, _m: &mut std::collections::HashSet<usize>) {}
}

// Counts rustc runs on the current thread, so tests can check that a test
// runs rustc exactly once. Each test runs on its own thread.
#[cfg(test)]
thread_local! {
    static RUSTC_RUNS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

fn count_rustc_run() {
    #[cfg(test)]
    RUSTC_RUNS.with(|n| n.set(n.get() + 1));
}

#[track_caller]
fn run_rustc_backend(input: &str, expect: BackendExpect) {
    count_rustc_run();
    let (success, stderr) = formality_rust::to_rust::test_util::run_rustc(input);
    match expect {
        BackendExpect::Ok => {
            assert!(
                success,
                "expected `rustc` to succeed but it failed:\n{stderr}"
            );
        }
        BackendExpect::Err(e) => {
            assert!(
                !success,
                "expected `rustc` to fail but it succeeded:\n{stderr}"
            );
            let normalized = formality_core::test_util::normalize_paths(&stderr);
            e.assert_eq(&normalized);
        }
    }
}

/// Runs rustc on `input` and checks the result against `expect`. Does nothing
/// if there is no expectation.
///
/// Called once per test, after formality's own checks. For `borrowck_ok` and
/// `borrowck_err` that means after every borrowck mode has run: rustc's answer
/// doesn't depend on formality's mode, so it is checked once and separately
/// from the per-mode results.
///
/// `BackendExpect` is not `Clone` and is taken by value, and `borrowck_run`
/// only borrows `self`, so this can't be called once per mode.
#[track_caller]
fn check_rustc(input: &str, expect: Option<BackendExpect>) {
    if let Some(expect) = expect {
        run_rustc_backend(input, expect);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use formality_core::test;

    /// Number of rustc runs so far on this test's thread.
    fn rustc_runs() -> usize {
        RUSTC_RUNS.with(|n| n.get())
    }

    /// Accepted by formality in every mode, and by rustc.
    const ACCEPTED: &str = crates![crate Foo {
        fn foo() -> u32 {
            let x: u32 = 0_u32;
            return x;
        }
    }];

    /// Rejected by formality in every mode, and by rustc (use after move).
    const MOVED: &str = crates![crate Foo {
        struct Datum { value: u32 }

        fn foo() -> Datum {
            let x: Datum = Datum { value: 0_u32 };
            let y: Datum = x;
            let z: Datum = x;
            return z;
        }
    }];

    /// Rejected by formality's NLL mode but accepted by its Polonius modes
    /// (NLL problem case #3). rustc, which uses NLL, rejects it.
    const NLL_ONLY_ERROR: &str = crates![crate Foo {
        struct Map { }

        fn foo<'a>(m: &'a mut Map) -> &'a mut Map {
            exists<'r0, 'r1> {
                let n: &'r0 mut Map = &'r0 mut *m;
                if false {
                    return n;
                } else {
                    let o: &'r1 mut Map = &'r1 mut *m;
                    return o;
                }
            }
        }
    }];

    #[test]
    fn borrowck_ok_checks_rustc_ok_once() {
        let before = rustc_runs();
        FormalityTest::new(ACCEPTED)
            .rustc_ok()
            .skip_execute()
            .borrowck_ok();
        assert_eq!(rustc_runs() - before, 1);
    }

    // rustc accepts this program, so the check panics before the message is compared.
    #[test]
    #[should_panic(expected = "expected `rustc` to fail but it succeeded")]
    fn borrowck_ok_enforces_rustc_err() {
        FormalityTest::new(ACCEPTED)
            .rustc_err(expect_test::expect![[r#"..."#]])
            .skip_execute()
            .borrowck_ok();
    }

    #[test]
    fn borrowck_err_checks_rustc_err_once() {
        let before = rustc_runs();
        FormalityTest::new(MOVED)
            .rustc_err(expect_test::expect![[r#"
                error[E0382]: use of moved value: `x`
                 --> lib.rs
                  |
                6 |     let mut x: Datum = Datum { value: 0_u32 };
                  |         ----- move occurs because `x` has type `Datum`, which does not implement the `Copy` trait
                7 |     let mut y: Datum = x;
                  |                        - value moved here
                8 |     let mut z: Datum = x;
                  |                        ^ value used here after move
                  |
                note: if `Datum` implemented `Clone`, you could clone the value
                 --> lib.rs
                  |
                1 | pub struct Datum {
                  | ^^^^^^^^^^^^^^^^ consider implementing `Clone` for this type
                ...
                7 |     let mut y: Datum = x;
                  |                        - you could clone this value

                For more information about this error, try `rustc --explain E0382`.
                error: could not compile `Foo` (lib) due to 1 previous error
            "#]])
            .borrowck_err(BorrowCheckFailure::All, expect_test::expect![[r#"
                the rule "access_permitted" at (nll.rs) failed because
                  condition evaluated to false: `match access.kind
                  {
                      AccessKind::Write =>
                      check_place_writable(&state, &access.place.to_place_expression()),
                      AccessKind::Read | AccessKind::Move =>
                      check_place_initialized(&state, &access.place.to_place_expression()),
                  }`"#]]);
        assert_eq!(rustc_runs() - before, 1);
    }

    #[test]
    #[should_panic(expected = "expected `rustc` to succeed but it failed")]
    fn borrowck_err_enforces_rustc_ok() {
        FormalityTest::new(MOVED).rustc_ok().borrowck_err(
            BorrowCheckFailure::All,
            expect_test::expect![[r#"
                the rule "access_permitted" at (nll.rs) failed because
                  condition evaluated to false: `match access.kind
                  {
                      AccessKind::Write =>
                      check_place_writable(&state, &access.place.to_place_expression()),
                      AccessKind::Read | AccessKind::Move =>
                      check_place_initialized(&state, &access.place.to_place_expression()),
                  }`"#]],
        );
    }

    #[test]
    fn rustc_expectation_is_independent_of_borrowck_modes() {
        let before = rustc_runs();
        FormalityTest::new(NLL_ONLY_ERROR)
            .rustc_err(expect_test::expect![[r#"
                error[E0499]: cannot borrow `*m` as mutable more than once at a time
                 --> lib.rs
                  |
                3 | pub fn foo<'a00>(mut m: &'a00 mut Map) -> &'a00 mut Map {
                  |            ---- lifetime `'a00` defined here
                4 |     {
                5 |         let mut n: &'_ mut Map = &mut *m;
                  |                                  ------- first mutable borrow occurs here
                6 |         if false {
                7 |             return n;
                  |                    - returning this value requires that `*m` is borrowed for `'a00`
                8 |         } else {
                9 |             let mut o: &'_ mut Map = &mut *m;
                  |                                      ^^^^^^^ second mutable borrow occurs here

                For more information about this error, try `rustc --explain E0499`.
                error: could not compile `Foo` (lib) due to 1 previous error
            "#]])
            .borrowck_err(BorrowCheckFailure::Nll, expect_test::expect![[r#"
                the rule "borrow of disjoint places" at (nll.rs) failed because
                  condition evaluated to false: `place_disjoint_from_place(&loan.place, &access.place)`
                    &loan.place = *(m : &!lt_1 mut Map) : <&!lt_1 mut Map as Derefable>::Target
                    &access.place = *(m : &!lt_1 mut Map) : <&!lt_1 mut Map as Derefable>::Target

                the rule "loan_cannot_outlive" at (nll.rs) failed because
                  condition evaluated to false: `!outlived_by_loan.contains(&lifetime.upcast())`
                    outlived_by_loan = {!lt_1, ?lt_2, ?lt_3}
                    &lifetime.upcast() = !lt_1

                the rule "loan_not_required_by_universal_regions" at (nll.rs) failed because
                  condition evaluated to false: `outlived_by_loan.iter().all(|p| match p
                  {
                      Parameter::Ty(_) => false, Parameter::Lt(lt) => match lt.as_ref()
                      {
                          Lt::Static => false, Lt::Variable(Variable::UniversalVar(_)) => false,
                          Lt::Variable(Variable::ExistentialVar(_)) => true,
                          Lt::Variable(Variable::BoundVar(_)) =>
                          panic!("cannot outlive a bound var"), Lt::Erased => true,
                      }, Parameter::Const(_) => panic!("cannot outlive a constant"),
                  })`

                the rule "write-indirect" at (nll.rs) failed because
                  pattern `TypedPlaceExpressionData::Deref(place_loaned_ref)` did not match value `m`

                the rule "write-indirect" at (nll.rs) failed because
                  condition evaluated to false: `place_accessed.is_prefix_of(place_loaned_ref)`
                    place_accessed = *(m : &!lt_1 mut Map) : <&!lt_1 mut Map as Derefable>::Target
                    place_loaned_ref = m : &!lt_1 mut Map"#]]);
        assert_eq!(rustc_runs() - before, 1);
    }
}
