//! Render scraped judgments + coverage data into markdown.
//!
//! Positive coverage is rule-level (a proof means every premise held).
//! Negative coverage is premise-level: for each fallible premise we report
//! whether some test failed trying to prove it.

use crate::jsonl::{Coverage, TestLoc};
use crate::scrape::{Judgment, Premise, Rule};
use anyhow::{Context, Result};
use std::collections::HashSet;
use std::path::Path;

/// Render the top-level coverage table. Each covered cell links to a per-cell
/// detail page (see [`render_detail_pages_for`]) listing the tests involved.
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
            let pos = positive_cell(cov, &j.name, &r.name);
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
        let pos = positive_cell(cov, &j.name, &r.name);
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
                neg = premise_negative_cell(cov, &j.name, &j.file, &r.name, p),
            ));
        }
    }
    s
}

/// A generated per-cell detail page. `slug` is the filename stem (cells link to
/// `./{slug}.md`); `title` is a short heading for the mdbook sidebar; `content`
/// is the markdown body.
pub struct DetailPage {
    pub slug: String,
    pub title: String,
    pub content: String,
}

/// Build the detail pages for one judgment: one per covered rule (positive) and
/// one per fallible premise that was negatively tested. Cells produced by
/// `positive_cell` / `premise_negative_cell` link to exactly these pages.
pub fn render_detail_pages_for(
    j: &Judgment,
    cov: &Coverage,
    github_base: Option<&str>,
) -> Vec<DetailPage> {
    let mut pages = Vec::new();
    for r in &j.rules {
        // Positive: every test that exercised this rule.
        if let Some(locs) = cov.positive_tests(&j.name, &r.name) {
            if !locs.is_empty() {
                let mut content = format!(
                    "# Positive coverage: `{}` / `{}`\n\n{} {} exercised this rule:\n\n",
                    j.name,
                    r.name,
                    locs.len(),
                    plural(locs.len()),
                );
                for loc in locs {
                    content.push_str(&test_list_item(github_base, loc));
                    content.push('\n');
                }
                pages.push(DetailPage {
                    slug: pos_detail_slug(&j.name, &r.name),
                    title: format!("{} / {} (positive)", j.name, r.name),
                    content,
                });
            }
        }

        // Negative: every test that failed proving a given fallible premise.
        for p in &r.premises {
            if !p.fallible {
                continue;
            }
            let tests = cov.negative_premise_tests(&j.file, p.line);
            if tests.is_empty() {
                continue;
            }
            let mut content = format!(
                "# Negative coverage: `{}` / `{}` / premise `{}`\n\nPremise at line {}.",
                j.name,
                r.name,
                premise_label(&p.raw_text),
                p.line,
            );
            let causes = cov.premise_causes_for(&j.file, p.line);
            if !causes.is_empty() {
                let joined = causes.into_iter().collect::<Vec<_>>().join(", ");
                content.push_str(&format!(" Observed failure causes: {joined}."));
            }
            content.push_str(&format!(
                "\n\n{} {} failed proving this premise:\n\n",
                tests.len(),
                plural(tests.len()),
            ));
            for loc in &tests {
                content.push_str(&test_list_item(github_base, loc));
                content.push('\n');
            }
            pages.push(DetailPage {
                slug: neg_detail_slug(&j.name, &r.name, p.line),
                title: format!("{} / {} premise@{} (negative)", j.name, r.name, p.line),
                content,
            });
        }
    }
    pages
}

/// Positive-coverage cell for a rule: `✗` if no test exercised it, otherwise a
/// `[N tests]` link to a detail page listing every test that did.
fn positive_cell(cov: &Coverage, judgment: &str, rule: &str) -> String {
    match cov.positive_tests(judgment, rule) {
        Some(locs) if !locs.is_empty() => {
            let n = locs.len();
            format!(
                "[{n} {tests}](./{slug}.md)",
                tests = plural(n),
                slug = pos_detail_slug(judgment, rule),
            )
        }
        _ => "✗".to_string(),
    }
}

/// `"test"` for one, `"tests"` otherwise.
fn plural(n: usize) -> &'static str {
    if n == 1 {
        "test"
    } else {
        "tests"
    }
}

/// A bullet-list entry for `loc`: a GitHub link when `github_base` is set, or a
/// plain `file:line` when it is not.
fn test_list_item(github_base: Option<&str>, loc: &TestLoc) -> String {
    match github_base {
        Some(base) => format!(
            "- [{file}:{line}]({base}/{file}#L{line})",
            file = loc.file,
            line = loc.line,
        ),
        None => format!("- {file}:{line}", file = loc.file, line = loc.line),
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

/// Subpage cell for a single premise: `N/A` if infallible, a `[N tests]` link
/// to a detail page (plus the observed clause-cause tags) if some test failed
/// proving it, else `✗`.
fn premise_negative_cell(
    cov: &Coverage,
    judgment: &str,
    judgment_file: &str,
    rule: &str,
    premise: &Premise,
) -> String {
    if !premise.fallible {
        return "N/A".to_string();
    }
    let tests = cov.negative_premise_tests(judgment_file, premise.line);
    if tests.is_empty() {
        return "✗".to_string();
    }
    let link = format!(
        "[{n} {tests_word}](./{slug}.md)",
        n = tests.len(),
        tests_word = plural(tests.len()),
        slug = neg_detail_slug(judgment, rule, premise.line),
    );
    let causes = cov.premise_causes_for(judgment_file, premise.line);
    if causes.is_empty() {
        link
    } else {
        let joined = causes.into_iter().collect::<Vec<_>>().join(", ");
        format!("{link} ({joined})")
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
pub fn write_all(
    out_dir: &Path,
    judgments: &[Judgment],
    cov: &Coverage,
    github_base: Option<&str>,
) -> Result<()> {
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

        for page in render_detail_pages_for(j, cov, github_base) {
            if !seen_slugs.insert(page.slug.clone()) {
                eprintln!(
                    "warning: slug collision for coverage detail page `{}`, it will overwrite a sibling",
                    page.slug,
                );
            }
            std::fs::write(out_dir.join(format!("{}.md", page.slug)), page.content)?;
        }
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

/// Filename stem for a rule's positive detail page. Must match the link emitted
/// by `positive_cell`.
fn pos_detail_slug(judgment: &str, rule: &str) -> String {
    format!("{}__{}__pos", slug(judgment), anchor(rule))
}

/// Filename stem for a premise's negative detail page. Must match the link
/// emitted by `premise_negative_cell`.
fn neg_detail_slug(judgment: &str, rule: &str, premise_line: u32) -> String {
    format!("{}__{}__p{premise_line}__neg", slug(judgment), anchor(rule))
}
