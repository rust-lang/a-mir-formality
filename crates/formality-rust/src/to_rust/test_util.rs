//! This module provides helper macros and functions for asserting that
//! Formality code behaves the same as equivalent surface-level Rust code.

use crate::{
    rust::term,
    to_rust::{check_workspace, context::Context, crates::build_crates},
};

/// Asserts that the given Formality input is translated into the expected
/// Rust code. Only a single crate is supported.
#[macro_export]
macro_rules! assert_rust {
    ($input:tt, $expected:expr) => {{
        $crate::to_rust::test_util::assert_rust(stringify!($input), $expected);
    }};
}

#[track_caller]
pub fn assert_rust(input: &str, expected: expect_test::Expect) {
    let program = term(input);
    let mut ctx = Context::default();
    let rust = build_crates(&mut ctx, &program)
        .unwrap()
        .into_iter()
        .next()
        .unwrap()
        .1;
    expected.assert_eq(&rust);
}

/// Runs rustc on the given Formality program and returns the captured
/// stderr.
pub fn run_rustc(input: &str) -> (bool, String) {
    let mut tmp_dir = tempfile::TempDir::with_prefix("formality-").unwrap();
    if keep_dir() {
        tmp_dir.disable_cleanup(true);
    }
    let crates = term(input);
    let result = check_workspace(&crates, tmp_dir.path()).unwrap();
    let stderr = String::from_utf8_lossy(&result.stderr).into_owned();
    (result.status.success(), stderr)
}

fn keep_dir() -> bool {
    0 < std::env::var("FORMALITY_KEEP_DIR")
        .map(|s| s.parse::<i32>())
        .unwrap_or(Ok(0))
        .unwrap_or_default()
}
