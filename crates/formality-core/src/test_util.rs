//! Utilities for writing tests against formality-core.
//! We include utility functions for two recommended testing patterns.
//!
//! # Writing `#[test]` tests
//!
//! We recommend the following:
//!
//! 1. Use [tracing][] for your own logging statements.
//! 2. Add  `use formality_core::test` at the top of the file, which enables logging and lets you use `RUST_LOG=debug` or `RUST_LOG=trace` to debug your tests.
//! 3. If asserting the result of a judgment, call the `assert_ok` or `assert_err` methods on the returned [`ProvenSet`](crate::proven_set::ProvenSet) value.
//! 4. If invoking a function that returns `Fallible<()>`, adds calls to the `assert_ok` or `assert_err` methods in the [`ResultTestExt`] trait.
//! 5. All the `assert_ok`/`assert_err` methods take a [`expect_test::Expect`][] value, which you can produce with `expect_test::expect![]` macro.
//!    If using rust-analyzer, you can place the cursor on the `expect` token and run `rust-analyzer: run` to update the expected value
//!    automatically (or use `UPDATE_EXPECT=1` when running from the command line).
//!
//! Example:
//!
//! ```rust
//! use formality_core::test;
//! use formality_core::Fallible;
//! use formality_core::test_util::ResultTestExt;
//!
//! #[test]
//! fn example_test() {
//!     let result: Fallible<i32> = Ok(123);
//!     result.assert_ok(expect_test::expect!["123"]);
//! }
//! ```
//!
//! # Writing standalone tests with `ui_test`
//!
//! TODO: nicely package up `ui_test` to do normalization etc.
//!
//! [ui_test]: https://crates.io/crates/ui_test
//! [expect-test]: https://crates.io/crates/expect-test
//! [tracing]: https://crates.io/crates/tracing

use std::fmt::{Debug, Display};

use crate::judgment::FailedJudgment;

/// Converts `s` to a string and replaces path/line/column patterns like `src/blah/foo.rs:22:33` in the string
/// with just the filename `foo.rs`. This makes error messages resilient against changes to the source code
/// while still showing which file the error came from.
pub fn normalize_paths(s: impl Display) -> String {
    let s = s.to_string();
    // Capture the filename (without path) and strip line:column
    // Handle both forward slashes (Unix) and backslashes (Windows) as path separators
    let re = regex::Regex::new(r"\((?:[^()]+[/\\])?([^()/\\]+\.rs):\d+:\d+\)").unwrap();
    re.replace_all(&s, "($1)").to_string()
}

/// Format an error, extracting just the leaf failures if it contains a FailedJudgment.
/// This provides a concise view of what actually failed rather than the full nested tree.
/// Walks the error chain to find a FailedJudgment even if wrapped in context.
pub fn format_error_leaves(e: &anyhow::Error) -> String {
    // Try direct downcast to Box<FailedJudgment> first (how it's typically created)
    if let Some(failed) = e.downcast_ref::<Box<FailedJudgment>>() {
        return failed.format_leaves();
    }
    // Also try FailedJudgment directly in case it was wrapped differently
    if let Some(failed) = e.downcast_ref::<FailedJudgment>() {
        return failed.format_leaves();
    }
    // If no FailedJudgment found, fall back to debug format
    format!("{e:?}")
}

/// Extension methods for writing tests on functions that return [`Fallible`](crate::Fallible) values.
pub trait ResultTestExt<T, E> {
    /// Given a `Fallible<T>` value, assert that it is `Ok`. Panics if it is an error.
    fn assert_ok(self);

    /// Given a `Fallible<T>` value, assert that it is an error with the given string (after normalization).
    /// Returns `Ok(())` if the assertion succeeds, or panics if the assertion fails.
    fn assert_err(self, expect: expect_test::Expect);

    /// Given a `Fallible<T>` value, assert that it is an error with the given string (after normalization).
    /// Also assert that each of the strings in `must_have` appears somewhere within.
    /// Returns `Ok(())` if the assertion succeeds, or panics if the assertion fails.
    fn assert_has_err(self, expect: expect_test::Expect, must_have: &[&str]);
}

impl<T, E> ResultTestExt<T, E> for Result<T, E>
where
    T: Debug,
    E: Debug,
{
    #[track_caller]
    fn assert_ok(self) {
        match self {
            Ok(_) => {}
            Err(e) => {
                panic!("expected `Ok`, got `Err`: {e:?}");
            }
        }
    }

    #[track_caller]
    fn assert_err(self, expect: expect_test::Expect) {
        self.assert_has_err(expect, &[]);
    }

    #[track_caller]
    fn assert_has_err(self, expect: expect_test::Expect, must_have: &[&str]) {
        match self {
            Ok(v) => panic!("expected `Err`, got `Ok`:\n{v:?}"),
            Err(e) => {
                let output = normalize_paths(format!("{e:?}"));

                expect.assert_eq(&output);

                for s in must_have {
                    assert!(output.contains(s), "did not find {s:?} in the output");
                }
            }
        }
    }
}

/// Extension trait for anyhow::Result that extracts leaf failures for concise error display
pub trait AnyhowResultTestExt<T> {
    /// Assert that the result is an error, showing only the leaf failures
    fn assert_err_leaves(self, expect: expect_test::Expect);

    /// Assert that the result is an error with the given leaf failures, and check must_have strings
    fn assert_has_err_leaves(self, expect: expect_test::Expect, must_have: &[&str]);
}

impl<T: Debug> AnyhowResultTestExt<T> for anyhow::Result<T> {
    #[track_caller]
    fn assert_err_leaves(self, expect: expect_test::Expect) {
        self.assert_has_err_leaves(expect, &[]);
    }

    #[track_caller]
    fn assert_has_err_leaves(self, expect: expect_test::Expect, must_have: &[&str]) {
        match self {
            Ok(v) => panic!("expected `Err`, got `Ok`:\n{v:?}"),
            Err(e) => {
                // Extract just the leaf failures for a concise view
                let output = normalize_paths(format_error_leaves(&e));

                expect.assert_eq(&output);

                for s in must_have {
                    assert!(output.contains(s), "did not find {s:?} in the output");
                }
            }
        }
    }
}
