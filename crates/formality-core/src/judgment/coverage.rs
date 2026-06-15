//! Coverage tracking for judgment tests.
//!
//! When a test successfully proves a judgment, we walk its [`ProofTree`] and
//! record every `(judgment, rule)` pair that fired (positive coverage). When a
//! test asserts a judgment *fails*, we record the set of reasons it failed
//! (negative coverage) so we can tell which fallible premises are exercised.
//!
//! Records are appended one JSONL line per test to `target/test-coverage.jsonl`
//! and serialized via the [`CoverageRecord`] types below, which the
//! `formality-coverage` reader deserializes with the same definitions.
//!
//! Recording is best-effort: I/O errors are swallowed so coverage never fails
//! a test. Disable entirely with `FORMALITY_COVERAGE=0`. Redirect the output
//! directory with `FORMALITY_COVERAGE_DIR` (used by the integration tests).

use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::fs::OpenOptions;
use std::io::Write;
use std::panic::Location;
use std::path::PathBuf;

use super::{FailedJudgment, FailureReason, ProofTree};

/// A single (judgment, rule) pair as identified in positive coverage output.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug, Serialize, Deserialize)]
pub struct RuleId {
    pub judgment: String,
    pub rule: String,
    pub file: String,
    pub line: u32,
}

/// One JSONL line of coverage data. Shared between the writer (here) and the
/// `formality-coverage` reader so the schema can only ever change in one place.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum CoverageRecord {
    /// A test proved one or more judgments. We track only the rules that
    /// fired: a successful proof means every premise of those rules held, so
    /// premise-level detail adds nothing here.
    Positive {
        test_file: String,
        test_line: u32,
        test_column: u32,
        rules: BTreeSet<RuleId>,
    },

    /// A test asserted a judgment fails. We track the set of failure reasons
    /// (see [`FailureReason`]) so we can check that every fallible premise is
    /// exercised by some negative test.
    Negative {
        test_file: String,
        test_line: u32,
        test_column: u32,
        reasons: BTreeSet<FailureReason>,
    },
}

impl ProofTree {
    /// Walk this proof tree and return every distinct `(judgment, rule)` pair
    /// that fired. Nodes with no `rule_name` (e.g. `for_all`, `if`, `let` leaves)
    /// are skipped because they do not correspond to user-defined inference rules.
    pub fn collect_rules(&self) -> BTreeSet<RuleId> {
        let mut acc = BTreeSet::new();
        self.collect_rules_into(&mut acc);
        acc
    }

    fn collect_rules_into(&self, acc: &mut BTreeSet<RuleId>) {
        if let Some(rule) = self.rule_name {
            acc.insert(RuleId {
                judgment: self.judgment_name.clone(),
                rule: rule.to_string(),
                // `file!()` produces backslashes on Windows in some paths; normalize
                // so coverage records are portable across platforms.
                file: self.file.replace('\\', "/"),
                line: self.line,
            });
        }
        for child in &self.children {
            child.collect_rules_into(acc);
        }
    }
}

/// Record positive coverage for the currently-running test from one or more
/// proof trees.
///
/// The test is identified by `Location::caller()`, which (because this function
/// is `#[track_caller]` and the assertion helpers that call it are too) resolves
/// to the source location of the `#[test]` body that triggered the assertion.
#[track_caller]
pub fn record_coverage<'a>(trees: impl IntoIterator<Item = &'a ProofTree>) {
    if !coverage_enabled() {
        return;
    }

    let Some(path) = coverage_file_path() else {
        return;
    };

    let caller = Location::caller();

    let mut rules: BTreeSet<RuleId> = BTreeSet::new();
    for tree in trees {
        tree.collect_rules_into(&mut rules);
    }

    let record = CoverageRecord::Positive {
        test_file: caller.file().replace('\\', "/"),
        test_line: caller.line(),
        test_column: caller.column(),
        rules,
    };
    write_record(&path, &record);
}

/// Record negative coverage for the currently-running test from one or more
/// failed judgments. For each failure, the set of reasons it failed (see
/// [`FailedJudgment::collect_failure_reasons`]) is recorded.
#[track_caller]
pub fn record_negative_coverage<'a>(failures: impl IntoIterator<Item = &'a FailedJudgment>) {
    if !coverage_enabled() {
        return;
    }

    let Some(path) = coverage_file_path() else {
        return;
    };

    let caller = Location::caller();

    let mut reasons: BTreeSet<FailureReason> = BTreeSet::new();
    for failure in failures {
        reasons.extend(failure.collect_failure_reasons());
    }

    let record = CoverageRecord::Negative {
        test_file: caller.file().replace('\\', "/"),
        test_line: caller.line(),
        test_column: caller.column(),
        reasons,
    };
    write_record(&path, &record);
}

fn write_record(path: &PathBuf, record: &CoverageRecord) {
    if let Ok(line) = serde_json::to_string(record) {
        let _ = append_line(path, &line);
    }
}

fn coverage_enabled() -> bool {
    !matches!(std::env::var("FORMALITY_COVERAGE").as_deref(), Ok("0"))
}

fn coverage_file_path() -> Option<PathBuf> {
    let dir = coverage_dir()?;
    std::fs::create_dir_all(&dir).ok()?;
    Some(dir.join("test-coverage.jsonl"))
}

fn coverage_dir() -> Option<PathBuf> {
    if let Ok(d) = std::env::var("FORMALITY_COVERAGE_DIR") {
        return Some(PathBuf::from(d));
    }
    if let Ok(d) = std::env::var("CARGO_TARGET_DIR") {
        return Some(PathBuf::from(d));
    }
    // Walk up from CARGO_MANIFEST_DIR until we find a `target/` directory.
    let manifest = std::env::var("CARGO_MANIFEST_DIR").ok()?;
    let mut cur = PathBuf::from(manifest);
    loop {
        let candidate = cur.join("target");
        if candidate.is_dir() {
            return Some(candidate);
        }
        if !cur.pop() {
            return None;
        }
    }
}

// Append-mode writes of small payloads are atomic under PIPE_BUF on POSIX and
// have similar guarantees on Windows for ordinary files, so parallel `cargo test`
// processes can safely append to the same file without locking, provided each
// line stays small.
fn append_line(path: &PathBuf, line: &str) -> std::io::Result<()> {
    let mut f = OpenOptions::new().create(true).append(true).open(path)?;
    writeln!(f, "{line}")
}
