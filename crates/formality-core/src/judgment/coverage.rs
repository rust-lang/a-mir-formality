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
//!
//! # A worked example
//!
//! The snapshot types below ([`ProofTreeNode`], [`FailedTreeNode`]) are easiest
//! to read against a concrete judgment. Take a one-rule judgment:
//!
//! ```text
//! judgment_fn! {
//!     pub fn is_positive(x: i32) => () {
//!         debug(x)
//!
//!         (
//!             (if x > 0)
//!             ----------------------- ("positive")
//!             (is_positive(x) => ())
//!         )
//!     }
//! }
//! ```
//!
//! and two tests: a positive one that proves `is_positive(1)`, and a negative
//! one that asserts `is_positive(-1)` cannot be proven.
//!
//! The positive test fires the `"positive"` rule (its `if x > 0` premise
//! holds), so [`record_coverage`] stores one [`ProofTreeNode`]:
//!
//! ```text
//! ProofTreeNode { judgment: "is_positive", rule: Some("positive"), children: [] }
//! ```
//!
//! The `if x > 0` premise is a leaf that fired no rule of its own, so it is
//! dropped (see [`ProofTreeNode`]); only the rule application survives.
//!
//! The negative test fails because that rule's `if x > 0` premise is false, so
//! [`record_negative_coverage`] stores one [`FailedTreeNode`]:
//!
//! ```text
//! FailedTreeNode {
//!     judgment: "is_positive",
//!     rules: [FailedRuleNode { rule: Some("positive"), cause: "if_false", child: None }],
//! }
//! ```

use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::fs::OpenOptions;
use std::io::Write;
use std::panic::Location;
use std::path::PathBuf;

use super::proven_set::judgment_name_prefix;
use super::{FailedJudgment, FailedRule, FailureReason, ProofTree, RuleFailureCause};

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
    /// A test proved one or more judgments. We track the rules that fired (a
    /// successful proof means every premise of those rules held, so
    /// premise-level detail adds nothing here) and, so the report can render
    /// each test's proof, the success proof tree(s) themselves.
    Positive {
        test_file: String,
        test_line: u32,
        test_column: u32,
        rules: BTreeSet<RuleId>,
        /// The success proof tree(s) this test produced, as a structural
        /// snapshot (see [`ProofTreeNode`]). Defaulted so coverage files
        /// written before trees were recorded still deserialize.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        trees: Vec<ProofTreeNode>,
    },

    /// A test asserted a judgment fails. We track the set of failure reasons
    /// (see [`FailureReason`]) so we can check that every fallible premise is
    /// exercised by some negative test, and the failed proof tree(s) so the
    /// report can render where each test's proof broke.
    Negative {
        test_file: String,
        test_line: u32,
        test_column: u32,
        reasons: BTreeSet<FailureReason>,
        /// The failed proof tree(s) this test produced (see
        /// [`FailedTreeNode`]). Defaulted for forward compatibility, as above.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        trees: Vec<FailedTreeNode>,
    },
}

/// A structural snapshot of a [`ProofTree`] node for embedding in positive
/// coverage records. We keep the proof *shape* (judgment name, the rule that
/// fired, the source location, and children) and drop the per-node attributes
/// (the argument/result `Debug` dumps), which are large and which the report
/// does not render.
///
/// We also drop "leaf" premise nodes that fired no rule and have no children:
/// these are the macro's `if` / `let` / trivial side-conditions, whose names are
/// large `Debug` dumps of bound values and which carry no inference structure.
/// Every node we keep is therefore a rule application or a `for_all`, all of
/// which have small, stable names. This keeps the snapshot both lean and a
/// faithful tree of the rules that actually fired.
///
/// `rule` is owned because the live `ProofTree.rule_name` is `&'static str`,
/// which serde can serialize but cannot deserialize into; a mirror type lets
/// the report read trees back without changing the proving engine's hot type.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProofTreeNode {
    /// The name of the judgment this node proves, e.g. `"is_positive"` in the
    /// module example (the identifier after `fn` in the `judgment_fn!`).
    pub judgment: String,
    /// The rule that fired, e.g. `Some("positive")` (the name in quotes after
    /// the rule's line). `None` marks a node that fired no named rule but is
    /// kept because it groups children of its own, e.g. a `for_all` premise;
    /// `None` leaves with no children are dropped (see the type doc).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub rule: Option<String>,
    pub file: String,
    pub line: u32,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub children: Vec<ProofTreeNode>,
}

impl From<&ProofTree> for ProofTreeNode {
    fn from(t: &ProofTree) -> Self {
        let children = t
            .children
            .iter()
            .map(ProofTreeNode::from)
            .filter(|c| c.rule.is_some() || !c.children.is_empty())
            .collect();
        ProofTreeNode {
            judgment: t.judgment_name.clone(),
            rule: t.rule_name.map(str::to_string),
            file: t.file.replace('\\', "/"),
            line: t.line,
            children,
        }
    }
}

/// A structural snapshot of a [`FailedJudgment`] for embedding in negative
/// coverage records: the judgment that failed and the rules that were tried.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FailedTreeNode {
    pub judgment: String,
    pub file: String,
    pub line: u32,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub rules: Vec<FailedRuleNode>,
}

/// One tried-and-failed rule under a [`FailedTreeNode`]. `file`/`line` locate
/// the rule's blamed premise (the macro respans failures onto it, so they match
/// the premise position negative coverage is keyed by). A `failed_judgment`
/// cause is represented structurally via `child`; every other cause is terminal
/// and summarized by its `cause` discriminant tag, matching the deliberately
/// payload-free, run-to-run-stable design of [`FailureReason`].
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FailedRuleNode {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub rule: Option<String>,
    pub file: String,
    pub line: u32,
    pub cause: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub child: Option<Box<FailedTreeNode>>,
}

impl From<&FailedJudgment> for FailedTreeNode {
    fn from(j: &FailedJudgment) -> Self {
        FailedTreeNode {
            judgment: judgment_name_prefix(&j.judgment),
            file: j.location.file.replace('\\', "/"),
            line: j.location.line,
            rules: j.failed_rules.iter().map(FailedRuleNode::from).collect(),
        }
    }
}

impl From<&FailedRule> for FailedRuleNode {
    fn from(r: &FailedRule) -> Self {
        let child = match &r.cause {
            RuleFailureCause::FailedJudgment(inner) => {
                Some(Box::new(FailedTreeNode::from(&**inner)))
            }
            _ => None,
        };
        FailedRuleNode {
            rule: r.rule_name.clone(),
            file: r.file.replace('\\', "/"),
            line: r.line,
            cause: r.cause.discriminant_tag().to_string(),
            child,
        }
    }
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
    let mut snapshots: Vec<ProofTreeNode> = Vec::new();
    for tree in trees {
        tree.collect_rules_into(&mut rules);
        snapshots.push(ProofTreeNode::from(tree));
    }

    let record = CoverageRecord::Positive {
        test_file: caller.file().replace('\\', "/"),
        test_line: caller.line(),
        test_column: caller.column(),
        rules,
        trees: snapshots,
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
    let mut snapshots: Vec<FailedTreeNode> = Vec::new();
    for failure in failures {
        reasons.extend(failure.collect_failure_reasons());
        snapshots.push(FailedTreeNode::from(failure));
    }

    let record = CoverageRecord::Negative {
        test_file: caller.file().replace('\\', "/"),
        test_line: caller.line(),
        test_column: caller.column(),
        reasons,
        trees: snapshots,
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

// Parallel `cargo test` processes append to this file concurrently. A bare
// append is only atomic up to `PIPE_BUF` on POSIX, and an embedded proof tree
// easily exceeds that, so without coordination two appends could interleave and
// tear a line. We therefore hold an exclusive OS-level lock (`File::lock`,
// released when `f` is dropped) for the duration of the write, which serializes
// writers across processes. The record and its trailing newline are written in
// a single `write_all` so the locked region is one call. We open with `read`
// as well as `append` because Windows requires read access on the handle to
// take the lock.
fn append_line(path: &PathBuf, line: &str) -> std::io::Result<()> {
    let mut f = OpenOptions::new()
        .create(true)
        .append(true)
        .read(true)
        .open(path)?;
    f.lock()?;
    f.write_all(format!("{line}\n").as_bytes())
}
