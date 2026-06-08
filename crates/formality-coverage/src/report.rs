//! Render scraped judgments + coverage data into markdown.

use crate::jsonl::{Coverage, CoveredRule};
use crate::scrape::{Judgment, Rule};
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
        let implicit = if cov.implicit_no_match_observed(&j.file, &j.name) {
            "implicit no-match observed"
        } else {
            "-"
        };
        s.push_str(&format!(
            "| **[{name}](./{slug}.md)** | - | {implicit} |\n",
            name = j.name,
            slug = slug,
        ));
        for r in &j.rules {
            let pos = positive_mark(&cov.positive, &j.name, &r.name);
            let neg = negative_mark(cov, &j.file, r);
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
    if cov.implicit_no_match_observed(&j.file, &j.name) {
        s.push_str("_Implicit no-match observed: at least one test exercised this judgment with no matching rule._\n\n");
    }
    if j.rules.is_empty() {
        s.push_str("_No rules discovered._\n");
        return s;
    }
    s.push_str("| Rule | Line | Positive coverage | Negative coverage |\n");
    s.push_str("| --- | --- | --- | --- |\n");
    for r in &j.rules {
        let pos = positive_mark(&cov.positive, &j.name, &r.name);
        let neg = negative_cell(cov, &j.file, r);
        s.push_str(&format!(
            "| <a id=\"{anchor}\"></a>`{rname}` | {line} | {pos} | {neg} |\n",
            anchor = anchor(&r.name),
            rname = r.name,
            line = r.line,
            pos = pos,
            neg = neg,
        ));
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

fn negative_mark(cov: &Coverage, judgment_file: &str, rule: &Rule) -> &'static str {
    if !cov
        .negative_causes_for(judgment_file, &rule.name)
        .is_empty()
    {
        "✓"
    } else if is_unblameable(rule) {
        "N/A"
    } else {
        "✗"
    }
}

/// Per-judgment-subpage cell: shows `✓` plus the observed clause-cause tags,
/// e.g. `✓ (if_false, failed_judgment)`, `✗` if the rule was never blamed,
/// or `N/A` for rules that structurally cannot fail (all premises infallible).
fn negative_cell(cov: &Coverage, judgment_file: &str, rule: &Rule) -> String {
    let causes = cov.negative_causes_for(judgment_file, &rule.name);
    if !causes.is_empty() {
        let joined = causes.into_iter().collect::<Vec<_>>().join(", ");
        format!("✓ ({joined})")
    } else if is_unblameable(rule) {
        "N/A".to_string()
    } else {
        "✗".to_string()
    }
}

/// A rule is "unblameable" if every premise is infallible (or it has no
/// premises at all): once its conclusion matches, the rule must succeed,
/// so it can never appear in negative coverage. The report shows `N/A`
/// rather than `✗` to avoid flagging a structural impossibility as a gap.
fn is_unblameable(rule: &Rule) -> bool {
    rule.premises.iter().all(|p| !p.fallible)
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
                "warning: slug collision for judgment `{}` at {}:{} — subpage will overwrite a sibling",
                j.name, j.file, j.line,
            );
        }
        let body = render_subpage(j, cov);
        std::fs::write(out_dir.join(format!("{}.md", slug)), body)?;
    }
    Ok(())
}

fn slug(name: &str) -> String {
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
