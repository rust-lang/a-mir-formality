//! Render scraped judgments + positive-coverage data into markdown.

use crate::jsonl::CoveredRule;
use crate::scrape::Judgment;
use anyhow::{Context, Result};
use std::collections::{BTreeSet, HashSet};
use std::path::Path;

/// Render the top-level coverage table.
pub fn render_index(judgments: &[Judgment], covered: &BTreeSet<CoveredRule>) -> String {
    let mut s = String::new();
    s.push_str("# Coverage report\n\n");
    s.push_str("| Judgment/Rule | Positive coverage |\n");
    s.push_str("| --- | --- |\n");
    for j in judgments {
        let slug = slug(&j.name);
        s.push_str(&format!(
            "| **[{name}](./{slug}.md)** | - |\n",
            name = j.name,
            slug = slug,
        ));
        for r in &j.rules {
            let mark = covered_mark(covered, &j.name, &r.name);
            s.push_str(&format!(
                "| ↳ [{rname}](./{slug}.md#{anchor}) | {mark} |\n",
                rname = r.name,
                slug = slug,
                anchor = anchor(&r.name),
                mark = mark,
            ));
        }
    }
    s
}

/// Render one subpage per judgment.
pub fn render_subpage(j: &Judgment, covered: &BTreeSet<CoveredRule>) -> String {
    let mut s = String::new();
    s.push_str(&format!(
        "# Judgment `{}` at {}:{}\n\n",
        j.name, j.file, j.line,
    ));
    if j.rules.is_empty() {
        s.push_str("_No rules discovered._\n");
        return s;
    }
    s.push_str("| Rule | Line | Positive coverage |\n");
    s.push_str("| --- | --- | --- |\n");
    for r in &j.rules {
        let mark = covered_mark(covered, &j.name, &r.name);
        s.push_str(&format!(
            "| <a id=\"{anchor}\"></a>`{rname}` | {line} | {mark} |\n",
            anchor = anchor(&r.name),
            rname = r.name,
            line = r.line,
            mark = mark,
        ));
    }
    s
}

fn covered_mark(covered: &BTreeSet<CoveredRule>, judgment: &str, rule: &str) -> &'static str {
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

/// Write the index and per-judgment subpages into `out_dir`.
pub fn write_all(
    out_dir: &Path,
    judgments: &[Judgment],
    covered: &BTreeSet<CoveredRule>,
) -> Result<()> {
    std::fs::create_dir_all(out_dir).with_context(|| format!("creating {}", out_dir.display()))?;
    let index = render_index(judgments, covered);
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
        let body = render_subpage(j, covered);
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
