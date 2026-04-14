//! This module provides helper macros and functions for asserting that
//! Formality code behaves the same as equivalent surface-level Rust code.

use crate::{
    check::check_all_crates,
    rust::term,
    to_rust::{check_workspace, RustBuilder},
};

/// Asserts that the given Formality input is translated into the expected
/// Rust code. Only a single crate is supported.
///
/// The Formality `input` must be provided as a token tree. The `expected` Rust
/// output must be given as as string literal.
#[macro_export]
macro_rules! assert_rust {
    ($input:tt, $expected:literal) => {{
        $crate::to_rust::test_util::assert_rust(stringify!($input), $expected);
    }};
}

#[track_caller]
pub fn assert_rust(input: &str, expected: &str) {
    let program = term(input);
    let rust = RustBuilder::default()
        .build_crates(&program)
        .unwrap()
        .into_iter()
        .next()
        .unwrap()
        .1;
    let expected = expected.trim();
    assert_eq!(rust, expected);
}

/// Asserts that compiling the given Formality code as Rust code with `rustc`
/// succeeds.
///
/// By default, the generated Rust workspace is created in a temporary
/// directory and deleted after the assertion completes. If the environment
/// variable `FORMALITY_KEEP_DIR` is set to `1`, the temporary workspace is
/// preserved to allow inspection of the generated Rust code.
#[macro_export]
macro_rules! assert_rustc_success {
    ($input:tt) => {{
        $crate::to_rust::test_util::assert_rustc_success(stringify!($input));
    }};
}

#[track_caller]
pub fn assert_rustc_success(input: &str) {
    let mut tmp_dir = tempfile::TempDir::with_prefix("formality-").unwrap();
    if keep_dir() {
        tmp_dir.disable_cleanup(true);
    }

    let crates = term(input);
    let result = check_workspace(&crates, tmp_dir.path()).unwrap();
    let stderr = String::from_utf8_lossy(&result.stderr);
    assert!(result.status.success(), "{stderr}");
}

/// Asserts that compiling the given Formality code as Rust code with `rustc`
/// fails.
///
/// One or more expected rustc error codes may be provided to assert that the
/// failure occurs for the anticipated reasons.
///
/// By default, the generated Rust workspace is created in a temporary
/// directory and deleted after the assertion completes. If the environment
/// variable `FORMALITY_KEEP_DIR` is set to `1`, the temporary workspace is
/// preserved to allow inspection of the generated Rust code.
#[macro_export]
macro_rules! assert_rustc_error {
    ($input:tt) => {{
        $crate::to_rust::test_util::assert_rustc_error(stringify!($input), &[], tmp_dir.path());
    }};
    ($input:tt, [$($codes:expr),*]) => {{
        let args: &[&str] = &[
            $($codes)*
        ];
        $crate::to_rust::test_util::assert_rustc_error(stringify!($input), args, tmp_dir.path());
    }};
}

#[track_caller]
pub fn assert_rustc_error(input: &str, codes: &[&str]) {
    let mut tmp_dir = tempfile::TempDir::with_prefix("formality-").unwrap();
    if keep_dir() {
        tmp_dir.disable_cleanup(true);
    }
    let crates = term(input);
    let result = check_workspace(&crates, tmp_dir.path()).unwrap();
    assert!(!result.status.success());

    let stderr = String::from_utf8_lossy(&result.stderr);
    for code in codes {
        assert!(stderr.find(*code).is_some());
    }
}

/// Asserts that Formality and `rustc` arrive at the same conclusion about
/// whether the given program type-checks.
///
/// By default, the generated Rust workspace is created in a temporary
/// directory and deleted after the assertion completes. If the environment
/// variable `FORMALITY_KEEP_DIR` is set to `1`, the temporary workspace is
/// preserved to allow inspection of the generated Rust code.
#[macro_export]
macro_rules! assert_equivalence {
    ($input:tt) => {{
        $crate::to_rust::test_util::assert_equivalence(stringify!($input));
    }};
}

pub fn assert_equivalence(input: &str) {
    let mut tmp_dir = tempfile::TempDir::with_prefix("formality-").unwrap();
    if keep_dir() {
        tmp_dir.disable_cleanup(true);
    }
    let crates = term(input);
    let rustc_result = check_workspace(&crates, tmp_dir.path()).unwrap();
    let formality_result = check_all_crates(crates).check_proven();

    match formality_result {
        Ok(_) => assert!(rustc_result.status.success()),
        Err(_) => assert!(!rustc_result.status.success()),
    }
}

/// Returns `true` if the environment variable `FORMALITY_KEEP_DIR` is set to
/// `1`.
///
/// Otherwise, returns `false`.
///
/// This function is used to determine whether the temporary workspace
/// generated for Rust compilation should be preserved after running tests.
fn keep_dir() -> bool {
    0 < std::env::var("FORMALITY_KEEP_DIR")
        .map(|s| s.parse::<i32>())
        .unwrap_or(Ok(0))
        .unwrap_or_default()
}
