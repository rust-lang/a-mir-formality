//! Coverage tracking for positive judgment tests.
//!
//! When a test successfully proves a judgment, we walk its [`ProofTree`] and
//! record every `(judgment, rule)` pair that fired. Records are appended,
//! one JSONL line per test, to `target/test-coverage.jsonl`. The eventual goal
//! is to identify judgment rules that are not exercised by any positive test.
//!
//! Recording is best-effort: I/O errors are swallowed so coverage never fails
//! a test. Disable entirely with `FORMALITY_COVERAGE=0`. Redirect the output
//! directory with `FORMALITY_COVERAGE_DIR` (used by the integration tests).

use std::collections::BTreeSet;
use std::fs::OpenOptions;
use std::io::Write;
use std::panic::Location;
use std::path::PathBuf;

use super::{BlamedRule, FailedJudgment, ProofTree};

/// A single (judgment, rule) pair as identified in the coverage output.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct RuleId {
    pub judgment: String,
    pub rule: String,
    pub file: String,
    pub line: u32,
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

/// Record coverage for the currently-running test from one or more proof trees.
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

    let line = format_jsonl(caller, &rules);
    let _ = append_line(&path, &line);
}

/// Record negative coverage for the currently-running test from one or more
/// failed judgments. For each failure, every named rule that was "blamed"
/// (see [`FailedJudgment::collect_blamed_rules`]) is appended.
///
/// JSONL records produced here carry `"kind":"negative"` and a per-rule
/// `"cause"` tag. Positive records (written by [`record_coverage`]) omit the
/// `kind` field so existing data remains valid.
#[track_caller]
pub fn record_negative_coverage<'a>(failures: impl IntoIterator<Item = &'a FailedJudgment>) {
    if !coverage_enabled() {
        return;
    }

    let Some(path) = coverage_file_path() else {
        return;
    };

    let caller = Location::caller();

    let mut rules: BTreeSet<BlamedRule> = BTreeSet::new();
    for failure in failures {
        rules.extend(failure.collect_blamed_rules());
    }

    let line = format_negative_jsonl(caller, &rules);
    let _ = append_line(&path, &line);
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

fn format_jsonl(caller: &Location<'_>, rules: &BTreeSet<RuleId>) -> String {
    let mut s = String::new();
    s.push('{');
    s.push_str("\"test_file\":");
    push_json_str(&mut s, &caller.file().replace('\\', "/"));
    s.push_str(",\"test_line\":");
    s.push_str(&caller.line().to_string());
    s.push_str(",\"test_column\":");
    s.push_str(&caller.column().to_string());
    s.push_str(",\"rules\":[");
    for (i, r) in rules.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        s.push('{');
        s.push_str("\"judgment\":");
        push_json_str(&mut s, &r.judgment);
        s.push_str(",\"rule\":");
        push_json_str(&mut s, &r.rule);
        s.push_str(",\"file\":");
        push_json_str(&mut s, &r.file);
        s.push_str(",\"line\":");
        s.push_str(&r.line.to_string());
        s.push('}');
    }
    s.push_str("]}");
    s
}

fn format_negative_jsonl(caller: &Location<'_>, rules: &BTreeSet<BlamedRule>) -> String {
    let mut s = String::new();
    s.push('{');
    s.push_str("\"test_file\":");
    push_json_str(&mut s, &caller.file().replace('\\', "/"));
    s.push_str(",\"test_line\":");
    s.push_str(&caller.line().to_string());
    s.push_str(",\"test_column\":");
    s.push_str(&caller.column().to_string());
    s.push_str(",\"kind\":\"negative\",\"rules\":[");
    for (i, r) in rules.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        s.push('{');
        s.push_str("\"rule\":");
        push_json_str(&mut s, &r.rule);
        s.push_str(",\"file\":");
        push_json_str(&mut s, &r.file);
        s.push_str(",\"line\":");
        s.push_str(&r.line.to_string());
        s.push_str(",\"cause\":");
        push_json_str(&mut s, r.cause);
        s.push('}');
    }
    s.push_str("]}");
    s
}

fn push_json_str(out: &mut String, s: &str) {
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => {
                out.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => out.push(c),
        }
    }
    out.push('"');
}
