//! Discover the full set of judgments and rules in the source tree by
//! regex-scraping `judgment_fn!` macro invocations.
//!
//! This is intentionally lightweight: we do not parse Rust, we just find
//! `judgment_fn!` blocks and the `--- ("rule-name")` lines inside them. If
//! the macro syntax changes, the regexes here must follow.
//!
//! Block boundary caveat: each judgment's body is taken as the text from
//! `judgment_fn!` up to the next `judgment_fn!` (or EOF). A rule-shaped line
//! (`--- ("name")`) sitting outside a macro invocation would be misattributed.
//! In practice this pattern only appears inside `judgment_fn!`, so we accept
//! the trade-off and avoid Rust-aware brace balancing.

use anyhow::Result;
use regex::Regex;
use std::path::{Path, PathBuf};
use std::sync::LazyLock;
use walkdir::WalkDir;

static INVOCATION_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"judgment_fn!\s*\{").unwrap());
static FN_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"\b(?:pub\s+)?fn\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(").unwrap());
static RULE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"^\s*-{3,}\s*\(\s*"([^"]+)"\s*\)\s*$"#).unwrap());

/// A single judgment discovered by the scraper.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Judgment {
    pub name: String,
    pub file: String,
    pub line: u32,
    pub rules: Vec<Rule>,
}

/// A single inference rule inside a judgment.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Rule {
    pub name: String,
    pub line: u32,
}

/// Walk `root` recursively and scrape every `.rs` file for `judgment_fn!`
/// blocks. Results are sorted by `(file, line)` for stable output.
pub fn scrape_dir(root: &Path) -> Result<Vec<Judgment>> {
    let mut out = Vec::new();
    for entry in WalkDir::new(root) {
        let entry = match entry {
            Ok(e) => e,
            Err(err) => {
                eprintln!("warning: skipping entry under {}: {err}", root.display());
                continue;
            }
        };
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("rs") {
            continue;
        }
        let contents = std::fs::read_to_string(path)?;
        let rel = normalize_path(path, root);
        out.extend(scrape_text(&contents, &rel));
    }
    out.sort_by(|a, b| (a.file.as_str(), a.line).cmp(&(b.file.as_str(), b.line)));
    Ok(out)
}

fn normalize_path(path: &Path, root: &Path) -> String {
    let rel: PathBuf = path.strip_prefix(root).unwrap_or(path).to_path_buf();
    rel.to_string_lossy().replace('\\', "/")
}

/// Scrape one file's text. `file` is the path string to record in results.
pub fn scrape_text(text: &str, file: &str) -> Vec<Judgment> {
    let starts: Vec<usize> = INVOCATION_RE.find_iter(text).map(|m| m.start()).collect();
    let mut out = Vec::new();
    for (i, &start) in starts.iter().enumerate() {
        let end = starts.get(i + 1).copied().unwrap_or(text.len());
        let block = &text[start..end];

        let Some(cap) = FN_RE.captures(block) else {
            continue;
        };
        let name = cap.get(1).unwrap().as_str().to_string();
        let fn_offset_in_block = cap.get(0).unwrap().start();
        let fn_abs = start + fn_offset_in_block;
        let header_line = line_number(text, fn_abs);

        let mut rules = Vec::new();
        for (rel_line_idx, line) in block.lines().enumerate() {
            if let Some(c) = RULE_RE.captures(line) {
                let line_no = line_number(text, start) + rel_line_idx as u32;
                rules.push(Rule {
                    name: c.get(1).unwrap().as_str().to_string(),
                    line: line_no,
                });
            }
        }

        out.push(Judgment {
            name,
            file: file.to_string(),
            line: header_line,
            rules,
        });
    }
    out
}

fn line_number(text: &str, byte_offset: usize) -> u32 {
    let upto = &text[..byte_offset.min(text.len())];
    (upto.bytes().filter(|&b| b == b'\n').count() as u32) + 1
}
