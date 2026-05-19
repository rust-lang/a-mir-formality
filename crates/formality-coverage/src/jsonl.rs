//! Read the `test-coverage.jsonl` file produced by positive tests and
//! collapse it to the set of `(judgment, rule)` pairs that fired at least once.

use anyhow::{Context, Result};
use serde_json::Value;
use std::collections::BTreeSet;
use std::path::Path;

/// A `(judgment, rule)` pair as it appears in coverage data.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct CoveredRule {
    pub judgment: String,
    pub rule: String,
}

/// Parse the JSONL file at `path`, returning the deduplicated set of rules
/// that fired across all recorded tests. If the file does not exist, returns
/// an empty set (positive coverage is opt-in via `cargo test`).
pub fn read_positive(path: &Path) -> Result<BTreeSet<CoveredRule>> {
    let mut out = BTreeSet::new();
    if !path.exists() {
        return Ok(out);
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
    Ok(out)
}
