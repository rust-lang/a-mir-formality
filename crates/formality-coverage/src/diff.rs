//! Compare a PR's coverage summary against a baseline and render a markdown
//! report. The headline totals answer "did coverage move?"; the per-rule
//! sections answer the question that actually matters in review: "did this
//! change add or uncover a rule that no test exercises?"

use crate::summary::{CoverageSummary, RuleSummary, Totals};
use std::collections::BTreeMap;

/// A `(judgment, rule)` key, flattened out of the nested summary for diffing.
type RuleKey = (String, String);

/// The outcome of comparing two summaries.
pub struct DiffReport {
    pub base: Totals,
    pub head: Totals,
    /// Rules covered in the baseline but no longer covered: the regression
    /// signal worth surfacing loudest.
    pub regressed: Vec<RuleKey>,
    /// Rules that exist only in the head and are uncovered: added by this change
    /// with no test.
    pub new_uncovered: Vec<RuleKey>,
    /// Rules newly covered (either previously-uncovered or brand new): the wins.
    pub newly_covered: Vec<RuleKey>,
}

fn flatten(summary: &CoverageSummary) -> BTreeMap<RuleKey, RuleSummary> {
    let mut out = BTreeMap::new();
    for (jname, j) in &summary.judgments {
        for (rname, r) in &j.rules {
            out.insert((jname.clone(), rname.clone()), *r);
        }
    }
    out
}

pub fn diff(base: &CoverageSummary, head: &CoverageSummary) -> DiffReport {
    let base_rules = flatten(base);
    let head_rules = flatten(head);

    let mut regressed = Vec::new();
    let mut new_uncovered = Vec::new();
    let mut newly_covered = Vec::new();

    for (key, h) in &head_rules {
        match base_rules.get(key) {
            Some(b) => {
                if b.positive_covered && !h.positive_covered {
                    regressed.push(key.clone());
                } else if !b.positive_covered && h.positive_covered {
                    newly_covered.push(key.clone());
                }
            }
            None => {
                // Rule is new in this change.
                if h.positive_covered {
                    newly_covered.push(key.clone());
                } else {
                    new_uncovered.push(key.clone());
                }
            }
        }
    }

    DiffReport {
        base: base.totals,
        head: head.totals,
        regressed,
        new_uncovered,
        newly_covered,
    }
}

impl DiffReport {
    /// True when nothing about the diff should give a reviewer pause: no rule
    /// lost coverage and no newly added rule is untested.
    pub fn is_clean(&self) -> bool {
        self.regressed.is_empty() && self.new_uncovered.is_empty()
    }

    /// Render as GitHub-flavored markdown, suitable for `$GITHUB_STEP_SUMMARY`
    /// or a sticky PR comment.
    pub fn to_markdown(&self) -> String {
        let mut s = String::from("## Judgment coverage vs. baseline\n\n");

        s.push_str("| Metric | Baseline | This change | Δ |\n");
        s.push_str("| --- | --- | --- | --- |\n");
        push_total_row(
            &mut s,
            "Rules covered",
            self.base.positive_covered,
            self.head.positive_covered,
            self.head.rules,
            self.base.rules,
        );
        push_total_row(
            &mut s,
            "Premises covered",
            self.base.premises_covered,
            self.head.premises_covered,
            self.head.premises_fallible,
            self.base.premises_fallible,
        );
        s.push('\n');

        if self.is_clean() {
            s.push_str(
                "No rules lost coverage and every rule added by this change is tested. ✅\n",
            );
        }

        push_rule_section(
            &mut s,
            "⚠️ Rules that lost coverage",
            &self.regressed,
            "These rules were covered on the baseline but no test exercises them here.",
        );
        push_rule_section(
            &mut s,
            "⚠️ New rules with no test",
            &self.new_uncovered,
            "Added by this change and exercised by no positive test.",
        );
        push_rule_section(
            &mut s,
            "✅ Newly covered rules",
            &self.newly_covered,
            "Now exercised by at least one test.",
        );

        s
    }
}

fn push_total_row(
    s: &mut String,
    label: &str,
    base: usize,
    head: usize,
    head_total: usize,
    base_total: usize,
) {
    let delta = head as isize - base as isize;
    let sign = if delta > 0 { "+" } else { "" };
    s.push_str(&format!(
        "| {label} | {base}/{base_total} | {head}/{head_total} | {sign}{delta} |\n"
    ));
}

fn push_rule_section(s: &mut String, heading: &str, rules: &[RuleKey], blurb: &str) {
    if rules.is_empty() {
        return;
    }
    s.push_str(&format!("\n### {heading}\n\n{blurb}\n\n"));
    for (judgment, rule) in rules {
        s.push_str(&format!("- `{judgment}` / `{rule}`\n"));
    }
}
