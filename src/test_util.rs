use expect_test::Expect;
use formality_core::test_util::AnyhowResultTestExt;

use crate::{run_rustc, test_program_ok};

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
