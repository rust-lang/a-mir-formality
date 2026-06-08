//! Discover the full set of judgments and rules in the source tree by
//! parsing `judgment_fn!` macro invocations.
//!
//! Parsing is intentionally lightweight: we do not invoke a Rust parser, we
//! only scan for `judgment_fn!` and walk the body with brace/paren matching
//! that ignores strings and `//` line comments. If the macro syntax changes,
//! the logic here must follow.

use anyhow::Result;
use regex::Regex;
use std::path::{Path, PathBuf};
use std::sync::LazyLock;
use walkdir::WalkDir;

/// A single judgment discovered by the scraper.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Judgment {
    pub name: String,
    pub doc_comment: String,
    pub signature: String,
    pub file: String,
    pub line: u32,
    pub rules: Vec<Rule>,
}

/// A single inference rule inside a judgment.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Rule {
    pub name: String,
    pub raw_text: String,
    pub line: u32,
    /// Premises listed above the `---` separator, in source order.
    pub premises: Vec<Premise>,
}

/// One premise within a rule's body. Used by the report to decide whether
/// a rule can ever be "blamed" in negative coverage: a rule whose premises
/// are all infallible cannot fail once its conclusion matches, so an
/// uncovered cell renders as N/A rather than a gap.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Premise {
    pub raw_text: String,
    pub kind: PremiseKind,
    pub fallible: bool,
}

/// Coarse classification of premise syntax. Drives the fallibility heuristic
/// Niko outlined: `let` is infallible unless its body contains `?`; `if`,
/// `if let`, and judgment-call `(expr => binding)` premises are fallible.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PremiseKind {
    Let,
    If,
    IfLet,
    Judgment,
    /// Did not match any known premise shape — treated as fallible so we
    /// never mistakenly mark a rule as unblameable.
    Other,
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
        out.extend(parse_judgment_fns(&contents, &rel));
    }
    out.sort_by(|a, b| (a.file.as_str(), a.line).cmp(&(b.file.as_str(), b.line)));
    Ok(out)
}

fn normalize_path(path: &Path, root: &Path) -> String {
    let rel: PathBuf = path.strip_prefix(root).unwrap_or(path).to_path_buf();
    rel.to_string_lossy().replace('\\', "/")
}

/// Scrape one file's text. `file` is the path string recorded in results.
/// Kept as a thin alias for [`parse_judgment_fns`] so existing callers and
/// tests can continue to use it.
pub fn scrape_text(text: &str, file: &str) -> Vec<Judgment> {
    parse_judgment_fns(text, file)
}

/// Find every `judgment_fn! { ... }` invocation in `content` and parse it.
pub fn parse_judgment_fns(content: &str, file: &str) -> Vec<Judgment> {
    let mut judgments = Vec::new();
    let mut pos = 0;

    while let Some(start) = content[pos..].find("judgment_fn!") {
        let abs_start = pos + start;
        let jf_line = line_number(content, abs_start);

        let Some(brace_rel) = content[abs_start..].find('{') else {
            pos = abs_start + "judgment_fn!".len();
            continue;
        };
        let brace_start = abs_start + brace_rel;

        let Some(brace_end) = find_matching_brace(content, brace_start) else {
            pos = brace_start + 1;
            continue;
        };

        let block = &content[brace_start + 1..brace_end];
        let block_start_line = line_number(content, brace_start + 1);

        if let Some(judgment) = parse_single_judgment(
            block,
            &content[..abs_start],
            file,
            jf_line,
            block_start_line,
        ) {
            judgments.push(judgment);
        }

        pos = brace_end + 1;
    }

    judgments
}

fn parse_single_judgment(
    block: &str,
    preceding: &str,
    file: &str,
    jf_line: u32,
    block_start_line: u32,
) -> Option<Judgment> {
    static FN_RE: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new(r"(?s)(pub\s+)?fn\s+(\w+)\s*\((.*?)\)\s*=>\s*([^{]+)\{").unwrap()
    });

    let doc_comment = extract_doc_comment(preceding);
    let captures = FN_RE.captures(block)?;

    let name = captures.get(2)?.as_str().to_string();
    let params = captures.get(3)?.as_str();
    let return_ty = captures.get(4)?.as_str().trim();

    let clean_params = clean_params(params);
    let signature = format!("{name}({clean_params}) => {return_ty}");

    let rules = extract_rules(block, block_start_line);

    Some(Judgment {
        name,
        doc_comment,
        signature,
        file: file.to_string(),
        line: jf_line,
        rules,
    })
}

fn extract_doc_comment(preceding: &str) -> String {
    let mut doc_lines = Vec::new();
    for line in preceding.lines().rev() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("///") {
            doc_lines.push(rest.trim());
        } else if trimmed.is_empty() {
            continue;
        } else {
            break;
        }
    }
    doc_lines.reverse();
    doc_lines.join("\n")
}

fn clean_params(params: &str) -> String {
    params
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .collect::<Vec<_>>()
        .join(" ")
        .replace(" ,", ",")
}

fn extract_rules(block: &str, block_start_line: u32) -> Vec<Rule> {
    static SEPARATOR_RE: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r#"-{3,}\s*\("[^"]+"\)"#).unwrap());

    let mut rules = Vec::new();

    // Skip past an optional `debug(...)` directive at the top of the block.
    let after_debug_offset = if let Some(debug_start) = block.find("debug(") {
        let paren_close = find_matching_paren(block, debug_start + 5).unwrap_or(debug_start + 6);
        paren_close + 1
    } else {
        0
    };

    let after_debug = &block[after_debug_offset..];
    let after_debug_line_offset = block[..after_debug_offset].matches('\n').count() as u32;

    let mut pos = 0;
    while pos < after_debug.len() {
        let Some(trimmed_offset) = after_debug[pos..].find(|c: char| !c.is_whitespace()) else {
            break;
        };
        pos += trimmed_offset;

        if !after_debug[pos..].starts_with('(') {
            if let Some(nl) = after_debug[pos..].find('\n') {
                pos += nl + 1;
            } else {
                break;
            }
            continue;
        }

        let Some(close) = find_matching_paren(after_debug, pos) else {
            pos += 1;
            continue;
        };

        let rule_text = &after_debug[pos + 1..close];

        if let Some(rule_name) = extract_rule_name(rule_text) {
            let separator_line_offset = if let Some(m) = SEPARATOR_RE.find(rule_text) {
                rule_text[..m.start()].matches('\n').count() as u32
            } else {
                0
            };

            let rule_start_lines = after_debug[..pos + 1].matches('\n').count() as u32;
            let abs_line = block_start_line
                + after_debug_line_offset
                + rule_start_lines
                + separator_line_offset;

            let premises = extract_premises(rule_text);

            rules.push(Rule {
                name: rule_name,
                raw_text: clean_rule_text(rule_text),
                line: abs_line,
                premises,
            });
        }

        pos = close + 1;
    }

    rules
}

/// Walk the section of `rule_text` above the `---` separator and collect
/// each parenthesized premise in source order. A trailing `!` (match-commit
/// marker) is tolerated and dropped from the recorded text.
fn extract_premises(rule_text: &str) -> Vec<Premise> {
    static SEPARATOR_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r#"-{3,}"#).unwrap());

    let body_end = SEPARATOR_RE
        .find(rule_text)
        .map(|m| m.start())
        .unwrap_or(rule_text.len());
    let body = &rule_text[..body_end];

    let mut premises = Vec::new();
    let mut pos = 0;
    while pos < body.len() {
        let Some(trimmed) = body[pos..].find(|c: char| !c.is_whitespace()) else {
            break;
        };
        pos += trimmed;
        if !body[pos..].starts_with('(') {
            // Skip past any stray characters to the next line.
            if let Some(nl) = body[pos..].find('\n') {
                pos += nl + 1;
                continue;
            }
            break;
        }
        let Some(close) = find_matching_paren(body, pos) else {
            pos += 1;
            continue;
        };
        let inner = &body[pos + 1..close];
        let (kind, fallible) = classify_premise(inner);
        premises.push(Premise {
            raw_text: inner.trim().to_string(),
            kind,
            fallible,
        });
        pos = close + 1;
        // Skip an optional `!` match-commit marker after the closing paren.
        if body[pos..].starts_with('!') {
            pos += 1;
        }
    }
    premises
}

/// Classify a premise by its leading tokens and apply Niko's fallibility
/// heuristic: `let` is infallible *unless* its body contains a `?`; every
/// other recognized shape is fallible.
fn classify_premise(inner: &str) -> (PremiseKind, bool) {
    let trimmed = inner.trim_start();
    if let Some(rest) = trimmed.strip_prefix("let ") {
        let fallible = rest.contains('?');
        return (PremiseKind::Let, fallible);
    }
    if let Some(rest) = trimmed.strip_prefix("if ") {
        if rest.trim_start().starts_with("let ") {
            return (PremiseKind::IfLet, true);
        }
        return (PremiseKind::If, true);
    }
    if trimmed.contains("=>") {
        return (PremiseKind::Judgment, true);
    }
    (PremiseKind::Other, true)
}

fn extract_rule_name(rule_text: &str) -> Option<String> {
    static NAME_RE: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r#"-{3,}\s*\("([^"]+)"\)"#).unwrap());
    let captures = NAME_RE.captures(rule_text)?;
    Some(captures.get(1)?.as_str().to_string())
}

fn clean_rule_text(rule_text: &str) -> String {
    let lines: Vec<&str> = rule_text.lines().collect();

    let min_indent = lines
        .iter()
        .filter(|l| !l.trim().is_empty())
        .map(|l| l.len() - l.trim_start().len())
        .min()
        .unwrap_or(0);

    lines
        .iter()
        .map(|l| {
            if l.trim().is_empty() {
                ""
            } else if l.len() >= min_indent {
                &l[min_indent..]
            } else {
                l.trim()
            }
        })
        .filter(|l| !l.trim_start().starts_with("//"))
        .collect::<Vec<_>>()
        .join("\n")
        .trim()
        .to_string()
}

fn find_matching_brace(content: &str, open_pos: usize) -> Option<usize> {
    find_matching(content, open_pos, '{', '}')
}

fn find_matching_paren(content: &str, open_pos: usize) -> Option<usize> {
    find_matching(content, open_pos, '(', ')')
}

fn find_matching(content: &str, open_pos: usize, open: char, close: char) -> Option<usize> {
    let mut depth = 0;
    let mut in_string = false;
    let mut in_line_comment = false;
    let mut prev_char = '\0';

    for (i, ch) in content[open_pos..].char_indices() {
        if in_line_comment {
            if ch == '\n' {
                in_line_comment = false;
            }
            prev_char = ch;
            continue;
        }

        if in_string {
            if ch == '"' && prev_char != '\\' {
                in_string = false;
            }
            prev_char = ch;
            continue;
        }

        if ch == '/' && prev_char == '/' {
            in_line_comment = true;
            prev_char = ch;
            continue;
        }

        if ch == '"' {
            in_string = true;
            prev_char = ch;
            continue;
        }

        if ch == open {
            depth += 1;
        } else if ch == close {
            depth -= 1;
            if depth == 0 {
                return Some(open_pos + i);
            }
        }

        prev_char = ch;
    }

    None
}

fn line_number(text: &str, byte_offset: usize) -> u32 {
    let upto = &text[..byte_offset.min(text.len())];
    (upto.bytes().filter(|&b| b == b'\n').count() as u32) + 1
}
