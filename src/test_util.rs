use expect_test::Expect;
use formality_core::test_util::AnyhowResultTestExt;
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

/// Builder for a test program and its per-backend expectations.
pub struct FormalityTest {
    input: String,
    rustc_override: Option<BackendExpect>,
    skip_execute: bool,
    expected_output: Option<String>,
}

impl FormalityTest {
    pub fn new(input: impl Into<String>) -> Self {
        Self {
            input: input.into(),
            rustc_override: None,
            skip_execute: false,
            expected_output: None,
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
        } = self;

        let proof_tree = test_program_ok(&input).expect("expected program to pass");
        formality_core::judgment::coverage::record_coverage(std::iter::once(&proof_tree));

        if !skip_execute {
            let stdout = execute_program(&input);
            if let Some(expected) = expected_output {
                assert_eq!(stdout, expected, "program output mismatch");
            }
        }

        if let Some(expect) = rustc_override {
            run_rustc_backend(&input, expect);
        } else if run_rustc() {
            run_rustc_backend(&input, BackendExpect::Ok);
        }
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
        } = self;

        test_program_ok(&input).assert_err_leaves(expect);

        if let Some(rustc) = rustc_override {
            run_rustc_backend(&input, rustc);
        }
    }

    /// Assert formality accepts the program under every borrowck mode. After
    /// type-checking passes, also runs codegen + execution unless
    /// `.skip_execute()` was called.
    #[track_caller]
    pub fn borrowck_ok(self) {
        if let Err((failure, error)) = self.borrowck_run() {
            panic!(
                "expected every mode to accept this program, but it was {}:\n{}",
                failure.describe_rejection(),
                error,
            );
        }
    }

    /// Assert formality rejects the program with the given error for the passed
    /// `BorrowCheckFailure` mode and all more restrictive modes.
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
                        execute_program(&self.input);
                    }

                    if error.is_err() {
                        panic!("{mode:?} passed but a more permissive mode rejected it");
                    }
                }
                Err(e) => {
                    formality_core::test_util::record_negative_coverage_from_anyhow(&e);
                    let mut test_error = formality_core::test_util::normalize_paths(
                        formality_core::test_util::format_error_leaves(&e),
                    );
                    for gate in mode.feature_gates() {
                        test_error = test_error.replace(&format!(" #![feature({gate:?})]"), "");
                    }
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
fn execute_program(input: &str) -> String {
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
        minirust_rs::lang::Machine::new(program, stdout, stderr)
            .get_internal()
            .expect("machine creation failed");

    loop {
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

#[track_caller]
fn run_rustc_backend(input: &str, expect: BackendExpect) {
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
