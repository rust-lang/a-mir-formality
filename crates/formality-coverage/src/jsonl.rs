//! Read the `test-coverage.jsonl` file produced by tests and collapse it to
//! the deduplicated set of rules that fired (positive) and rules that were
//! blamed for a failure (negative).

use anyhow::{Context, Result};
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

/// A `(judgment, rule)` pair as it appears in positive coverage data.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct CoveredRule {
    pub judgment: String,
    pub rule: String,
}

/// A rule blamed for a negative-test failure. The recorded `file` is the
/// path the `file!()` macro produced (workspace-relative) and `line` is
/// the failing-premise line, not the rule's `--- ("name")` line — that's
/// where the macro respans the failure. Both fields are kept for
/// diagnostics; matching against the scraper uses `rule` + file suffix.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct BlamedRuleLoc {
    pub rule: String,
    pub file: String,
    pub line: u32,
}

/// Aggregated coverage data from a JSONL file.
#[derive(Default, Debug)]
pub struct Coverage {
    /// `(judgment, rule)` pairs that fired in at least one positive test.
    pub positive: BTreeSet<CoveredRule>,
    /// Each blamed rule location with the set of clause-cause tags observed.
    pub negative: BTreeMap<BlamedRuleLoc, BTreeSet<String>>,
}

impl Coverage {
    /// Look up causes for a scraped rule. Matches by rule name and file
    /// suffix (either side being a suffix of the other), tolerating the
    /// path-root difference between `file!()` and the scraper's
    /// `--src-root`.
    pub fn negative_causes_for(&self, judgment_file: &str, rule_name: &str) -> BTreeSet<String> {
        let mut out = BTreeSet::new();
        for (loc, causes) in &self.negative {
            if loc.rule != rule_name {
                continue;
            }
            if paths_overlap(&loc.file, judgment_file) {
                out.extend(causes.iter().cloned());
            }
        }
        out
    }
}

fn paths_overlap(a: &str, b: &str) -> bool {
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
        let Ok(value) = serde_json::from_str::<Value>(line) else {
            eprintln!(
                "warning: skipping malformed JSONL line {} in {}",
                i + 1,
                path.display(),
            );
            continue;
        };
        let Some(rules) = value.get("rules").and_then(|v| v.as_array()) else {
            continue;
        };
        let kind = value
            .get("kind")
            .and_then(|v| v.as_str())
            .unwrap_or("positive");
        match kind {
            "negative" => ingest_negative(rules, &mut cov.negative),
            _ => ingest_positive(rules, &mut cov.positive),
        }
    }
    Ok(cov)
}

/// Back-compat wrapper for tools that only want the positive set.
pub fn read_positive(path: &Path) -> Result<BTreeSet<CoveredRule>> {
    Ok(read(path)?.positive)
}

fn ingest_positive(rules: &[Value], out: &mut BTreeSet<CoveredRule>) {
    for rule in rules {
        let (Some(j), Some(r)) = (
            rule.get("judgment").and_then(|v| v.as_str()),
            rule.get("rule").and_then(|v| v.as_str()),
        ) else {
            continue;
        };
        out.insert(CoveredRule {
            judgment: j.to_string(),
            rule: r.to_string(),
        });
    }
}

fn ingest_negative(rules: &[Value], out: &mut BTreeMap<BlamedRuleLoc, BTreeSet<String>>) {
    for rule in rules {
        let (Some(rname), Some(file), Some(line)) = (
            rule.get("rule").and_then(|v| v.as_str()),
            rule.get("file").and_then(|v| v.as_str()),
            rule.get("line").and_then(|v| v.as_u64()),
        ) else {
            continue;
        };
        let loc = BlamedRuleLoc {
            rule: rname.to_string(),
            file: file.to_string(),
            line: line as u32,
        };
        let causes = out.entry(loc).or_default();
        if let Some(cause) = rule.get("cause").and_then(|v| v.as_str()) {
            causes.insert(cause.to_string());
        }
    }
}
