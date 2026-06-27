//! Read the `test-coverage.jsonl` file produced by tests and collapse it to
//! the deduplicated set of rules that fired (positive) and the per-premise
//! failure data (negative).
//!
//! Records are deserialized with [`CoverageRecord`], the same type the writer
//! in `formality-core` serializes, so the schema lives in exactly one place.

use anyhow::{Context, Result};
use formality_core::judgment::coverage::{CoverageRecord, FailedTreeNode, ProofTreeNode};
use formality_core::judgment::FailureReason;
use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

/// A `(judgment, rule)` pair as it appears in positive coverage data.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct CoveredRule {
    pub judgment: String,
    pub rule: String,
}

/// The source location of a `#[test]` that exercised some rule. Lets the report
/// turn a ✓ into a link to the test on GitHub.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct TestLoc {
    pub file: String,
    pub line: u32,
}

/// The source location of a premise that failed in some negative test. The
/// `file`/`line` come from the macro respanning the failure onto the failing
/// premise, so they match the premise's position in the judgment source.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct PremiseLoc {
    pub file: String,
    pub line: u32,
}

/// A judgment that was exercised but failed with no applicable rule.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct NoApplicableRuleLoc {
    pub judgment: String,
    pub file: String,
    pub line: u32,
}

/// Aggregated coverage data from a JSONL file.
#[derive(Default, Debug)]
pub struct Coverage {
    /// `(judgment, rule)` pairs that fired in at least one positive test, each
    /// mapped to the test locations that exercised them.
    pub positive: BTreeMap<CoveredRule, BTreeSet<TestLoc>>,
    /// Each failing-premise location with the set of clause-cause tags observed.
    pub negative_premises: BTreeMap<PremiseLoc, BTreeSet<String>>,
    /// Each failing-premise location with the test locations that failed there,
    /// so a negative cell can link to the tests that exercise it.
    pub negative_premise_tests: BTreeMap<PremiseLoc, BTreeSet<TestLoc>>,
    /// Judgments observed to fail with no applicable rule.
    pub no_applicable_rule: BTreeSet<NoApplicableRuleLoc>,
    /// Each positive test's success proof tree(s), so a positive detail page can
    /// render the proof every listed test produced.
    pub positive_trees: BTreeMap<TestLoc, Vec<ProofTreeNode>>,
    /// Each negative test's failed proof tree(s), so a negative detail page can
    /// render where each listed test's proof broke.
    pub negative_trees: BTreeMap<TestLoc, Vec<FailedTreeNode>>,
}

impl Coverage {
    /// Causes observed for a premise at `premise_line` in a file overlapping
    /// `judgment_file`. Matching is by line plus file-suffix overlap, which
    /// tolerates the path-root difference between `file!()` and the scraper's
    /// `--src-root`.
    pub fn premise_causes_for(&self, judgment_file: &str, premise_line: u32) -> BTreeSet<String> {
        let mut out = BTreeSet::new();
        for (loc, causes) in &self.negative_premises {
            if loc.line == premise_line && paths_overlap(&loc.file, judgment_file) {
                out.extend(causes.iter().cloned());
            }
        }
        out
    }

    /// Test locations that failed proving the premise at `premise_line` in a
    /// file overlapping `judgment_file`. Matching mirrors `premise_causes_for`.
    pub fn negative_premise_tests(
        &self,
        judgment_file: &str,
        premise_line: u32,
    ) -> BTreeSet<TestLoc> {
        let mut out = BTreeSet::new();
        for (loc, tests) in &self.negative_premise_tests {
            if loc.line == premise_line && paths_overlap(&loc.file, judgment_file) {
                out.extend(tests.iter().cloned());
            }
        }
        out
    }

    /// Test locations that exercised rule `rule` of `judgment`, if any fired.
    pub fn positive_tests(&self, judgment: &str, rule: &str) -> Option<&BTreeSet<TestLoc>> {
        self.positive.get(&CoveredRule {
            judgment: judgment.to_string(),
            rule: rule.to_string(),
        })
    }

    /// The success proof tree(s) recorded for `test`, or an empty slice if none.
    pub fn positive_trees_for(&self, test: &TestLoc) -> &[ProofTreeNode] {
        self.positive_trees.get(test).map_or(&[], Vec::as_slice)
    }

    /// The failed proof tree(s) recorded for `test`, or an empty slice if none.
    pub fn negative_trees_for(&self, test: &TestLoc) -> &[FailedTreeNode] {
        self.negative_trees.get(test).map_or(&[], Vec::as_slice)
    }

    /// True if `judgment_name` (in a file overlapping `judgment_file`) was
    /// observed to fail with no applicable rule.
    pub fn no_applicable_rule_observed(&self, judgment_file: &str, judgment_name: &str) -> bool {
        self.no_applicable_rule
            .iter()
            .any(|loc| loc.judgment == judgment_name && paths_overlap(&loc.file, judgment_file))
    }
}

pub(crate) fn paths_overlap(a: &str, b: &str) -> bool {
    a.ends_with(b) || b.ends_with(a)
}

/// Parse the JSONL file at `path`. If the file does not exist, returns an
/// empty `Coverage` (recording is opt-in via `cargo test`).
pub fn read(path: &Path) -> Result<Coverage> {
    let mut cov = Coverage::default();
    if !path.exists() {
        return Ok(cov);
    }
    let text =
        std::fs::read_to_string(path).with_context(|| format!("reading {}", path.display()))?;
    for (i, line) in text.lines().enumerate() {
        if line.trim().is_empty() {
            continue;
        }
        // Tolerate corrupted lines: parallel `cargo test` processes append
        // to this file concurrently, and on rare occasions two writes can
        // interleave. A bad line should never abort the report.
        let Ok(record) = serde_json::from_str::<CoverageRecord>(line) else {
            eprintln!(
                "warning: skipping malformed JSONL line {} in {}",
                i + 1,
                path.display(),
            );
            continue;
        };
        ingest(record, &mut cov);
    }
    Ok(cov)
}

/// Back-compat wrapper for tools that only want the set of covered rules,
/// without their test locations.
pub fn read_positive(path: &Path) -> Result<BTreeSet<CoveredRule>> {
    Ok(read(path)?.positive.into_keys().collect())
}

fn ingest(record: CoverageRecord, cov: &mut Coverage) {
    match record {
        CoverageRecord::Positive {
            test_file,
            test_line,
            rules,
            trees,
            ..
        } => {
            let loc = TestLoc {
                file: test_file,
                line: test_line,
            };
            for r in rules {
                cov.positive
                    .entry(CoveredRule {
                        judgment: r.judgment,
                        rule: r.rule,
                    })
                    .or_default()
                    .insert(loc.clone());
            }
            if !trees.is_empty() {
                cov.positive_trees.entry(loc).or_default().extend(trees);
            }
        }
        CoverageRecord::Negative {
            test_file,
            test_line,
            reasons,
            trees,
            ..
        } => {
            let test = TestLoc {
                file: test_file,
                line: test_line,
            };
            if !trees.is_empty() {
                cov.negative_trees
                    .entry(test.clone())
                    .or_default()
                    .extend(trees);
            }
            for reason in reasons {
                match reason {
                    FailureReason::Premise {
                        file, line, cause, ..
                    } => {
                        let ploc = PremiseLoc { file, line };
                        cov.negative_premises
                            .entry(ploc.clone())
                            .or_default()
                            .insert(cause);
                        cov.negative_premise_tests
                            .entry(ploc)
                            .or_default()
                            .insert(test.clone());
                    }
                    FailureReason::NoApplicableRule {
                        judgment,
                        file,
                        line,
                    } => {
                        cov.no_applicable_rule.insert(NoApplicableRuleLoc {
                            judgment,
                            file,
                            line,
                        });
                    }
                }
            }
        }
    }
}
