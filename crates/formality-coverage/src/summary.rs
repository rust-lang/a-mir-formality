//! A compact, machine-readable coverage summary and a baseline diff.
//!
//! The full coverage report is large: a detail page and proof tree for every
//! rule. For CI comparison we want something small and stable:
//! per-`(judgment, rule)` hit counts plus totals, keyed and ordered
//! deterministically so identical coverage yields byte-identical JSON. This is
//! the file the published book carries as a baseline and a PR build diffs
//! against.

use crate::jsonl::Coverage;
use crate::scrape::Judgment;
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::Path;

/// Bumped whenever the on-disk shape changes so a new build diffing against an
/// old published baseline can detect the mismatch instead of misreading it.
pub const SCHEMA_VERSION: u32 = 1;

/// The whole summary: workspace totals plus a per-judgment breakdown.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CoverageSummary {
    pub schema_version: u32,
    pub totals: Totals,
    /// Keyed by judgment name, ordered for byte-stable output. Two judgments
    /// with the same name (already a warned-about pathology in the report) would
    /// collide here; the first one built wins.
    pub judgments: BTreeMap<String, JudgmentSummary>,
}

/// Workspace-wide roll-up, so a diff can show a headline delta without walking
/// every rule.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Totals {
    pub rules: usize,
    /// Rules whose conclusion fired in at least one positive test.
    pub positive_covered: usize,
    /// Fallible premises across all rules (the negative-coverage denominator).
    pub premises_fallible: usize,
    /// Fallible premises observed to fail in at least one negative test.
    pub premises_covered: usize,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct JudgmentSummary {
    pub file: String,
    /// At least one test exercised the judgment with no matching rule.
    pub no_applicable_rule: bool,
    pub rules: BTreeMap<String, RuleSummary>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RuleSummary {
    pub positive_covered: bool,
    pub positive_tests: usize,
    pub premises_fallible: usize,
    pub premises_covered: usize,
}

/// Build a summary from the scraped judgments and recorded coverage, mirroring
/// the accounting the rendered report uses (see `report::negative_index_cell`
/// and `positive_num`).
pub fn build(judgments: &[Judgment], cov: &Coverage) -> CoverageSummary {
    let mut out: BTreeMap<String, JudgmentSummary> = BTreeMap::new();

    for j in judgments {
        let mut rules = BTreeMap::new();
        for r in &j.rules {
            let positive_tests = cov
                .positive_tests(&j.name, &r.name)
                .map_or(0, |locs| locs.len());
            let fallible: Vec<_> = r.premises.iter().filter(|p| p.fallible).collect();
            let premises_covered = fallible
                .iter()
                .filter(|p| !cov.premise_causes_for(&j.file, p.line).is_empty())
                .count();

            rules.insert(
                r.name.clone(),
                RuleSummary {
                    positive_covered: positive_tests > 0,
                    positive_tests,
                    premises_fallible: fallible.len(),
                    premises_covered,
                },
            );
        }

        let entry = JudgmentSummary {
            file: j.file.clone(),
            no_applicable_rule: cov.no_applicable_rule_observed(&j.file, &j.name),
            rules,
        };
        // First occurrence wins; a duplicate judgment name is a warned-about
        // pathology in the rendered report anyway.
        out.entry(j.name.clone()).or_insert(entry);
    }

    // Total from the deduped map, not the raw input: a duplicate-named judgment
    // (which the map collapses) must not inflate the counts, and the totals must
    // not depend on input order or on whether the caller pre-deduped. This keeps
    // a summary built from the CLI's `scrape_dir` output byte-identical to one
    // built from the preprocessor's, so the CI diff sees no phantom churn.
    let mut totals = Totals::default();
    for j in out.values() {
        for r in j.rules.values() {
            totals.rules += 1;
            if r.positive_covered {
                totals.positive_covered += 1;
            }
            totals.premises_fallible += r.premises_fallible;
            totals.premises_covered += r.premises_covered;
        }
    }

    CoverageSummary {
        schema_version: SCHEMA_VERSION,
        totals,
        judgments: out,
    }
}

/// Serialize to pretty JSON. Ordering is via `BTreeMap`, so identical coverage
/// produces byte-identical output and the CI diff sees no spurious churn.
pub fn write(path: &Path, summary: &CoverageSummary) -> Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("creating {}", parent.display()))?;
    }
    let json = serde_json::to_string_pretty(summary)?;
    std::fs::write(path, json).with_context(|| format!("writing {}", path.display()))?;
    Ok(())
}

/// Read a summary previously written by [`write`] (e.g. the published baseline).
pub fn read(path: &Path) -> Result<CoverageSummary> {
    let text =
        std::fs::read_to_string(path).with_context(|| format!("reading {}", path.display()))?;
    let summary: CoverageSummary =
        serde_json::from_str(&text).with_context(|| format!("parsing {}", path.display()))?;
    Ok(summary)
}
