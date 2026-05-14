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
}

impl FormalityTest {
    pub fn new(input: impl Into<String>) -> Self {
        Self {
            input: input.into(),
            rustc_override: None,
        }
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

    /// Assert formality accepts the program. Also runs rustc if overridden,
    /// or if `FORMALITY_RUN_RUSTC=1` (in which case rustc must also accept).
    #[track_caller]
    pub fn ok(self) {
        let Self {
            input,
            rustc_override,
        } = self;

        let proof_tree = test_program_ok(&input).expect("expected program to pass");
        formality_core::judgment::coverage::record_coverage(std::iter::once(&proof_tree));
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
        } = self;

        test_program_ok(&input).assert_err_leaves(expect);

        if let Some(rustc) = rustc_override {
            run_rustc_backend(&input, rustc);
        }
    }
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
