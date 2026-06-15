//! Render scraped judgments + coverage data into markdown.
//!
//! Positive coverage is rule-level (a proof means every premise held).
//! Negative coverage is premise-level: for each fallible premise we report
//! whether some test failed trying to prove it.

use crate::jsonl::{Coverage, CoveredRule};
use crate::scrape::{Judgment, Premise, Rule};
use anyhow::{Context, Result};
use std::collections::{BTreeSet, HashSet};
use std::path::Path;

/// Render the top-level coverage table.
pub fn render_index(judgments: &[Judgment], cov: &Coverage) -> String {
    let mut s = String::new();
    s.push_str("# Coverage report\n\n");
    s.push_str("| Judgment/Rule | Positive coverage | Negative coverage |\n");
    s.push_str("| --- | --- | --- |\n");
    for j in judgments {
        let slug = slug(&j.name);
        let neg = if cov.no_applicable_rule_observed(&j.file, &j.name) {
            "no applicable rule observed"
        } else {
            "-"
        };
        s.push_str(&format!(
            "| **[{name}](./{slug}.md)** | - | {neg} |\n",
            name = j.name,
            slug = slug,
        ));
        for r in &j.rules {
            let pos = positive_mark(&cov.positive, &j.name, &r.name);
            let neg = negative_index_cell(cov, &j.file, r);
            s.push_str(&format!(
                "| ↳ [{rname}](./{slug}.md#{anchor}) | {pos} | {neg} |\n",
                rname = r.name,
                slug = slug,
                anchor = anchor(&r.name),
                pos = pos,
                neg = neg,
            ));
        }
    }
    s
}

/// Render one subpage per judgment.
pub fn render_subpage(j: &Judgment, cov: &Coverage) -> String {
    let mut s = String::new();
    s.push_str(&format!(
        "# Judgment `{}` at {}:{}\n\n",
        j.name, j.file, j.line,
    ));
    if cov.no_applicable_rule_observed(&j.file, &j.name) {
        s.push_str("_No applicable rule observed: at least one test exercised this judgment with no matching rule._\n\n");
    }
    if j.rules.is_empty() {
        s.push_str("_No rules discovered._\n");
        return s;
    }

    // Rule-level positive coverage.
    s.push_str("## Rules\n\n");
    s.push_str("| Rule | Line | Positive coverage |\n");
    s.push_str("| --- | --- | --- |\n");
    for r in &j.rules {
        let pos = positive_mark(&cov.positive, &j.name, &r.name);
        s.push_str(&format!(
            "| <a id=\"{anchor}\"></a>`{rname}` | {line} | {pos} |\n",
            anchor = anchor(&r.name),
            rname = r.name,
            line = r.line,
            pos = pos,
        ));
    }

    // Premise-level negative coverage.
    s.push_str("\n## Premises (negative coverage)\n\n");
    s.push_str("| Rule | Premise | Line | Negatively tested |\n");
    s.push_str("| --- | --- | --- | --- |\n");
    for r in &j.rules {
        if r.premises.is_empty() {
            s.push_str(&format!(
                "| `{rname}` | _(no premises)_ | {line} | N/A |\n",
                rname = r.name,
                line = r.line,
            ));
            continue;
        }
        for p in &r.premises {
            s.push_str(&format!(
                "| `{rname}` | `{premise}` | {line} | {neg} |\n",
                rname = r.name,
                premise = premise_label(&p.raw_text),
                line = p.line,
                neg = premise_negative_cell(cov, &j.file, p),
            ));
        }
    }
    s
}

fn positive_mark(covered: &BTreeSet<CoveredRule>, judgment: &str, rule: &str) -> &'static str {
    let hit = covered.contains(&CoveredRule {
        judgment: judgment.to_string(),
        rule: rule.to_string(),
    });
    if hit {
        "✓"
    } else {
        "✗"
    }
}

/// Index cell for a rule's negative coverage: a `covered/total` count over
/// the rule's fallible premises, or `N/A` when the rule has none (so it can
/// never fail once its conclusion matches).
fn negative_index_cell(cov: &Coverage, judgment_file: &str, rule: &Rule) -> String {
    let fallible: Vec<&Premise> = rule.premises.iter().filter(|p| p.fallible).collect();
    if fallible.is_empty() {
        return "N/A".to_string();
    }
    let covered = fallible
        .iter()
        .filter(|p| !cov.premise_causes_for(judgment_file, p.line).is_empty())
        .count();
    format!("{covered}/{}", fallible.len())
}

/// Subpage cell for a single premise: `N/A` if infallible, `✓` plus the
/// observed clause-cause tags if some test failed proving it, else `✗`.
fn premise_negative_cell(cov: &Coverage, judgment_file: &str, premise: &Premise) -> String {
    if !premise.fallible {
        return "N/A".to_string();
    }
    let causes = cov.premise_causes_for(judgment_file, premise.line);
    if causes.is_empty() {
        "✗".to_string()
    } else {
        let joined = causes.into_iter().collect::<Vec<_>>().join(", ");
        format!("✓ ({joined})")
    }
}

/// Collapse a premise's source text to a single line and escape `|` so it
/// can sit in a markdown table cell.
fn premise_label(raw: &str) -> String {
    raw.split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
        .replace('|', "\\|")
}

/// Write the index and per-judgment subpages into `out_dir`.
pub fn write_all(out_dir: &Path, judgments: &[Judgment], cov: &Coverage) -> Result<()> {
    std::fs::create_dir_all(out_dir).with_context(|| format!("creating {}", out_dir.display()))?;
    let index = render_index(judgments, cov);
    std::fs::write(out_dir.join("coverage.md"), index)?;

    // Two judgments with the same name (or names that differ only by characters
    // we collapse into `_`) would clobber each other's subpage. Warn so the
    // collision is at least visible.
    let mut seen_slugs: HashSet<String> = HashSet::new();
    for j in judgments {
        let slug = slug(&j.name);
        if !seen_slugs.insert(slug.clone()) {
            eprintln!(
                "warning: slug collision for judgment `{}` at {}:{}, subpage will overwrite a sibling",
                j.name, j.file, j.line,
            );
        }
        let body = render_subpage(j, cov);
        std::fs::write(out_dir.join(format!("{}.md", slug)), body)?;
    }
    Ok(())
}

pub fn slug(name: &str) -> String {
    name.chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect()
}

fn anchor(name: &str) -> String {
    name.chars()
        .map(|c| match c {
            c if c.is_ascii_alphanumeric() => c.to_ascii_lowercase(),
            ' ' | '-' | '_' => '-',
            _ => '-',
        })
        .collect()
}
