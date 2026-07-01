//! Render scraped judgments + coverage data into markdown.
//!
//! Positive coverage is rule-level (a proof means every premise held).
//! Negative coverage is premise-level: for each fallible premise we report
//! whether some test failed trying to prove it.
//!
//! The index ([`render_index`]) is a plain markdown table. Each per-judgment
//! subpage ([`render_subpage`]) is a "code view": the rule's source rendered
//! with three columns (line number, coverage count, source line). The number on
//! a rule's conclusion is positive coverage; the number on a premise is negative
//! coverage. Both link to a per-cell detail page ([`render_detail_pages_for`])
//! that lists each individual test with its source location and source.

use crate::jsonl::{paths_overlap, Coverage, TestLoc};
use crate::scrape::{Judgment, Premise, Rule};
use anyhow::{Context, Result};
use formality_core::judgment::coverage::{FailedRuleNode, FailedTreeNode, ProofTreeNode};
use std::collections::HashSet;
use std::path::Path;

/// Inlined stylesheet for the code-view subpages. Emitted once per subpage so
/// the CLI report ([`write_all`]) and the mdbook preprocessor render
/// identically without any extra book configuration. Colors fall back
/// gracefully when the mdbook theme variables are absent (e.g. the standalone
/// CLI output viewed as raw HTML).
///
/// Kept as a single line-broken HTML block with no blank lines: a blank line
/// would terminate the surrounding HTML block under CommonMark and leak the
/// rest as literal markdown.
const STYLE: &str = "<style>\n\
.cov-rule{border:1px solid var(--quote-border,#d0d0d0);border-radius:6px;margin:1rem 0;overflow:hidden}\n\
.cov-rule-head{padding:.4rem .8rem;background:var(--quote-bg,#f6f7f9);font-weight:600}\n\
table.cov-code{width:100%;border-collapse:collapse;font-family:var(--mono-font,monospace);font-size:.85em;margin:0}\n\
table.cov-code td{padding:.15rem .6rem;border:0}\n\
table.cov-code th{padding:.15rem .6rem;border:0;border-bottom:1px solid var(--quote-border,#d0d0d0);color:#888;font-weight:600;font-size:.9em}\n\
.cov-ln{text-align:right;color:#999;user-select:none;width:3em;white-space:nowrap}\n\
.cov-num{text-align:right;width:3.5em;white-space:nowrap;font-weight:600}\n\
.cov-num.pos a{color:#1a7f37}\n\
.cov-num.neg a{color:#b35900}\n\
.cov-none{color:#bbb}\n\
.cov-na{color:#bbb;font-weight:400}\n\
.cov-src-line{white-space:pre-wrap}\n\
tr.cov-sep td{color:#999}\n\
tr.cov-concl{background:rgba(127,127,127,.08)}\n\
</style>\n";

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

/// Render one subpage per judgment as a code view. `link_ext` is the extension
/// used for the links from coverage numbers to detail pages: `"md"` for the
/// standalone CLI report (viewed as markdown) and `"html"` for the mdbook
/// preprocessor (mdbook only rewrites `.md`→`.html` for markdown-syntax links,
/// not for the raw-HTML `<a>` we emit here).
pub fn render_subpage(j: &Judgment, cov: &Coverage, link_ext: &str) -> String {
    let mut s = String::new();
    s.push_str(&format!(
        "# Judgment `{}` at {}:{}\n\n",
        j.name, j.file, j.line,
    ));
    if !j.signature.is_empty() {
        s.push_str("**Signature:**\n\n```rust,ignore\n");
        s.push_str(&j.signature);
        s.push_str("\n```\n\n");
    }
    if cov.no_applicable_rule_observed(&j.file, &j.name) {
        s.push_str("_No applicable rule observed: at least one test exercised this judgment with no matching rule._\n\n");
    }
    if j.rules.is_empty() {
        s.push_str("_No rules discovered._\n");
        return s;
    }

    s.push_str(
        "The number on each rule's conclusion is **positive** coverage; the number on each \
         premise is **negative** coverage. Click a number to browse the tests.\n\n",
    );
    s.push_str(STYLE);
    s.push('\n');
    for r in &j.rules {
        s.push_str(&render_rule_block(j, r, cov, link_ext));
        s.push('\n');
    }
    s
}

/// Render one rule as a three-column code view: line number, coverage count,
/// source line. Premises (above the separator) carry their negative coverage;
/// the conclusion (below it) carries the rule's positive coverage. Emitted as a
/// single blank-line-free HTML block (see [`STYLE`]).
fn render_rule_block(j: &Judgment, r: &Rule, cov: &Coverage, link_ext: &str) -> String {
    let mut s = String::new();
    s.push_str(&format!(
        "<div class=\"cov-rule\" id=\"{}\">\n",
        anchor(&r.name)
    ));
    s.push_str(&format!(
        "<div class=\"cov-rule-head\"><code>{}</code></div>\n",
        html_escape(&r.name),
    ));
    s.push_str("<table class=\"cov-code\">\n");
    s.push_str(
        "<thead><tr><th class=\"cov-ln\">Line</th><th class=\"cov-num\">Coverage</th>\
         <th class=\"cov-src-line\">Source</th></tr></thead>\n",
    );

    // Premises: negative coverage.
    for p in &r.premises {
        s.push_str(&format!(
            "<tr><td class=\"cov-ln\">{ln}</td><td class=\"cov-num neg\">{num}</td>\
             <td class=\"cov-src-line\">{code}</td></tr>\n",
            ln = p.line,
            num = negative_num(cov, j, r, p, link_ext),
            code = html_escape(&format!("({})", p.raw_text)),
        ));
    }

    // Separator carrying the rule name, mirroring the source syntax.
    s.push_str(&format!(
        "<tr class=\"cov-sep\"><td class=\"cov-ln\"></td><td class=\"cov-num\"></td>\
         <td class=\"cov-src-line\">{}</td></tr>\n",
        html_escape(&format!("──────── (\"{}\")", r.name)),
    ));

    // Conclusion: positive coverage. The conclusion sits just below the
    // separator, so its source line is `r.line + 1` (`r.line` is the separator).
    let concl = conclusion_of(&r.raw_text).unwrap_or_else(|| format!("({} => …)", j.name));
    s.push_str(&format!(
        "<tr class=\"cov-concl\"><td class=\"cov-ln\">{ln}</td><td class=\"cov-num pos\">{num}</td>\
         <td class=\"cov-src-line\">{code}</td></tr>\n",
        ln = r.line + 1,
        num = positive_num(cov, j, r, link_ext),
        code = html_escape(&concl),
    ));

    s.push_str("</table>\n</div>\n");
    s
}

/// The conclusion (everything below the `---` separator) of a rule's source
/// text, with surrounding whitespace trimmed. `None` when `raw_text` has no
/// separator (e.g. synthetic fixtures with empty source).
fn conclusion_of(raw_text: &str) -> Option<String> {
    let lines: Vec<&str> = raw_text.lines().collect();
    let sep = lines
        .iter()
        .position(|l| l.trim_start().starts_with("---"))?;
    let concl = lines[sep + 1..].join("\n");
    let concl = concl.trim();
    (!concl.is_empty()).then(|| concl.to_string())
}

/// The positive-coverage number cell for a rule's conclusion: a link to the
/// rule's detail page, or `✗` if no test exercised it.
fn positive_num(cov: &Coverage, j: &Judgment, r: &Rule, link_ext: &str) -> String {
    match cov.positive_tests(&j.name, &r.name) {
        Some(locs) if !locs.is_empty() => format!(
            "<a href=\"./{slug}.{ext}\">{n}</a>",
            slug = pos_detail_slug(&j.name, &r.name),
            ext = link_ext,
            n = locs.len(),
        ),
        _ => "<span class=\"cov-none\">✗</span>".to_string(),
    }
}

/// The negative-coverage number cell for a premise: `N/A` if the premise is
/// infallible, a link to the premise's detail page if some test failed proving
/// it (with the observed failure causes in the link title), else `✗`.
fn negative_num(cov: &Coverage, j: &Judgment, r: &Rule, p: &Premise, link_ext: &str) -> String {
    if !p.fallible {
        return "<span class=\"cov-na\">N/A</span>".to_string();
    }
    let tests = cov.negative_premise_tests(&j.file, p.line);
    if tests.is_empty() {
        return "<span class=\"cov-none\">✗</span>".to_string();
    }
    let causes = cov.premise_causes_for(&j.file, p.line);
    let title = if causes.is_empty() {
        String::new()
    } else {
        format!(
            " title=\"failure causes: {}\"",
            html_escape(&causes.into_iter().collect::<Vec<_>>().join(", ")),
        )
    };
    format!(
        "<a href=\"./{slug}.{ext}\"{title}>{n}</a>",
        slug = neg_detail_slug(&j.name, &r.name, p.line),
        ext = link_ext,
        title = title,
        n = tests.len(),
    )
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
/// `positive_num` / `negative_num` link to exactly these pages. Each page lists
/// the tests with, for each, its source location and (when `source_root` is set
/// and the file is readable) the test function's source inline.
pub fn render_detail_pages_for(
    j: &Judgment,
    cov: &Coverage,
    github_base: Option<&str>,
    source_root: Option<&Path>,
) -> Vec<DetailPage> {
    let mut pages = Vec::new();
    for r in &j.rules {
        // Positive: every test that exercised this rule.
        if let Some(locs) = cov.positive_tests(&j.name, &r.name) {
            if !locs.is_empty() {
                let mut content = format!("# Positive coverage: `{}` / `{}`\n\n", j.name, r.name);
                content.push_str(&format!(
                    "{} {} exercised this rule:\n\n",
                    locs.len(),
                    plural(locs.len()),
                ));
                content.push_str(&test_list(
                    github_base,
                    source_root,
                    locs,
                    TreeSection::Positive(cov),
                ));
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
            content.push_str(&test_list(
                github_base,
                source_root,
                &tests,
                TreeSection::Negative {
                    cov,
                    judgment_file: &j.file,
                    premise_line: p.line,
                },
            ));
            pages.push(DetailPage {
                slug: neg_detail_slug(&j.name, &r.name, p.line),
                title: format!("{} / {} premise@{} (negative)", j.name, r.name, p.line),
                content,
            });
        }
    }
    pages
}

/// Which proof tree to render under each test in a detail page.
enum TreeSection<'a> {
    /// Positive page: render each test's success proof tree (the rules it fired).
    Positive(&'a Coverage),
    /// Negative page for one premise: render each test's failed proof tree,
    /// pruned to the stacks that blame the premise at `judgment_file`/
    /// `premise_line` (we show only the stacks that involve this premise).
    Negative {
        cov: &'a Coverage,
        judgment_file: &'a str,
        premise_line: u32,
    },
}

/// How many of a cell's tests get their proof tree rendered inline. A hot rule
/// fires in hundreds of tests; rendering every tree would make the page (and the
/// mdbook search index) enormous, so we show trees for the first few only. The
/// source location and source are still listed for every test (they are cheap).
const MAX_TREES_PER_CELL: usize = 10;

/// A flat list of tests: for each, its source location (linked to GitHub when
/// `github_base` is set), the test function's source in a code block (when
/// `source_root` is set and the file is readable), and (for the first
/// [`MAX_TREES_PER_CELL`]) the test's proof tree in a collapsed disclosure.
/// Plain markdown, so the location links rewrite to `.html` under mdbook like
/// any other markdown link.
fn test_list<'a>(
    github_base: Option<&str>,
    source_root: Option<&Path>,
    tests: impl IntoIterator<Item = &'a TestLoc>,
    trees: TreeSection<'_>,
) -> String {
    let tests: Vec<&TestLoc> = tests.into_iter().collect();
    let mut s = String::new();
    for (i, loc) in tests.iter().enumerate() {
        let label = format!("{}:{}", loc.file, loc.line);
        s.push_str("---\n\n");
        match github_base {
            Some(base) => s.push_str(&format!(
                "**Source location:** [{label}]({base}/{file}#L{line})\n\n",
                label = label,
                base = base,
                file = loc.file,
                line = loc.line,
            )),
            None => s.push_str(&format!("**Source location:** {label}\n\n")),
        }
        if let Some(root) = source_root {
            if let Some(src) = extract_test_source(root, &loc.file, loc.line) {
                s.push_str("```rust,ignore\n");
                s.push_str(&src);
                s.push_str("\n```\n\n");
            }
        }
        if i < MAX_TREES_PER_CELL {
            s.push_str(&tree_details(&trees, loc));
        } else if i == MAX_TREES_PER_CELL {
            s.push_str(&format!(
                "_Proof trees omitted for the remaining {} tests._\n\n",
                tests.len() - MAX_TREES_PER_CELL,
            ));
        }
    }
    s
}

/// The collapsed proof-tree disclosure for one test, or an empty string when no
/// tree was recorded (or, on a negative page, none of the test's failed stacks
/// involve the premise this page is about).
fn tree_details(trees: &TreeSection<'_>, loc: &TestLoc) -> String {
    match trees {
        TreeSection::Positive(cov) => proof_tree_details(cov.positive_trees_for(loc)),
        TreeSection::Negative {
            cov,
            judgment_file,
            premise_line,
        } => {
            let pruned: Vec<FailedTreeNode> = cov
                .negative_trees_for(loc)
                .iter()
                .filter_map(|t| prune_failed(t, judgment_file, *premise_line))
                .collect();
            failed_tree_details(&pruned)
        }
    }
}

/// Wrap pre-rendered tree text in a collapsed `<details>` disclosure. Emitted as
/// one raw-HTML block with no internal blank lines: a blank line would close the
/// HTML block under CommonMark and leak the rest as literal markdown.
fn tree_disclosure(summary: &str, tree_text: &str) -> String {
    format!(
        "<details>\n<summary>{summary}</summary>\n<pre class=\"cov-tree\">\n{body}</pre>\n</details>\n\n",
        summary = summary,
        body = html_escape(tree_text),
    )
}

/// Largest proof tree we render inline. Where-clause solving recurses deeply,
/// so a single success tree can reach thousands of nodes, which is neither
/// readable nor cheap to ship in the search index. We render the first
/// [`MAX_TREE_NODES`] (depth-first) and note how many were elided.
const MAX_TREE_NODES: usize = 200;

/// The success proof tree(s) for one test, or `""` if none were recorded.
fn proof_tree_details(nodes: &[ProofTreeNode]) -> String {
    if nodes.is_empty() {
        return String::new();
    }
    let total: usize = nodes.iter().map(count_proof_nodes).sum();
    let mut text = String::new();
    let mut budget = MAX_TREE_NODES;
    for n in nodes {
        render_proof_node(n, "", &mut text, &mut budget);
    }
    append_truncation_note(&mut text, total);
    tree_disclosure("Proof tree", &text)
}

fn count_proof_nodes(n: &ProofTreeNode) -> usize {
    1 + n.children.iter().map(count_proof_nodes).sum::<usize>()
}

fn render_proof_node(n: &ProofTreeNode, prefix: &str, out: &mut String, budget: &mut usize) {
    if *budget == 0 {
        return;
    }
    *budget -= 1;
    let rule = n
        .rule
        .as_deref()
        .map(|r| format!(" ({r})"))
        .unwrap_or_default();
    out.push_str(&format!(
        "{prefix}└─ {judgment}{rule} at {file}:{line}\n",
        judgment = n.judgment,
        rule = rule,
        file = short_file(&n.file),
        line = n.line,
    ));
    let child_prefix = format!("{prefix}   ");
    for c in &n.children {
        render_proof_node(c, &child_prefix, out, budget);
    }
}

/// The failed proof tree(s) for one test, already pruned to the relevant
/// premise, or `""` if nothing survived the pruning.
fn failed_tree_details(nodes: &[FailedTreeNode]) -> String {
    if nodes.is_empty() {
        return String::new();
    }
    let total: usize = nodes.iter().map(count_failed_nodes).sum();
    let mut text = String::new();
    let mut budget = MAX_TREE_NODES;
    for n in nodes {
        render_failed_node(n, "", &mut text, &mut budget);
    }
    append_truncation_note(&mut text, total);
    tree_disclosure("Failed proof tree", &text)
}

fn count_failed_nodes(j: &FailedTreeNode) -> usize {
    1 + j.rules.iter().map(count_failed_rule_nodes).sum::<usize>()
}

fn count_failed_rule_nodes(r: &FailedRuleNode) -> usize {
    1 + r.child.as_deref().map_or(0, count_failed_nodes)
}

fn render_failed_node(j: &FailedTreeNode, prefix: &str, out: &mut String, budget: &mut usize) {
    if *budget == 0 {
        return;
    }
    *budget -= 1;
    out.push_str(&format!(
        "{prefix}└─ {judgment} failed at {file}:{line}\n",
        judgment = j.judgment,
        file = short_file(&j.file),
        line = j.line,
    ));
    let child_prefix = format!("{prefix}   ");
    for r in &j.rules {
        render_failed_rule(r, &child_prefix, out, budget);
    }
}

fn render_failed_rule(r: &FailedRuleNode, prefix: &str, out: &mut String, budget: &mut usize) {
    if *budget == 0 {
        return;
    }
    *budget -= 1;
    let label = match &r.rule {
        Some(name) => format!("rule \"{name}\""),
        None => "rule".to_string(),
    };
    match &r.child {
        // A nested judgment failure: recurse to show where it broke.
        Some(child) => {
            out.push_str(&format!(
                "{prefix}└─ {label} at {file}:{line}\n",
                label = label,
                file = short_file(&r.file),
                line = r.line,
            ));
            let child_prefix = format!("{prefix}   ");
            render_failed_node(child, &child_prefix, out, budget);
        }
        // A terminal failure: show the cause tag.
        None => out.push_str(&format!(
            "{prefix}└─ {label} at {file}:{line} (failed: {cause})\n",
            label = label,
            file = short_file(&r.file),
            line = r.line,
            cause = r.cause,
        )),
    }
}

/// Append a "N of M nodes shown" note when a tree was capped at
/// [`MAX_TREE_NODES`].
fn append_truncation_note(text: &mut String, total: usize) {
    if total > MAX_TREE_NODES {
        text.push_str(&format!("… ({MAX_TREE_NODES} of {total} nodes shown)\n"));
    }
}

/// Prune a failed judgment tree to only the stacks that blame the premise at
/// `judgment_file`/`premise_line`. The blamed premise is a failed rule at that
/// location; we keep it (with its full subtree) plus every ancestor on the path
/// to it, and drop sibling stacks that never reach it.
fn prune_failed(
    j: &FailedTreeNode,
    judgment_file: &str,
    premise_line: u32,
) -> Option<FailedTreeNode> {
    let rules: Vec<FailedRuleNode> = j
        .rules
        .iter()
        .filter_map(|r| prune_failed_rule(r, judgment_file, premise_line))
        .collect();
    (!rules.is_empty()).then(|| FailedTreeNode {
        judgment: j.judgment.clone(),
        file: j.file.clone(),
        line: j.line,
        rules,
    })
}

fn prune_failed_rule(
    r: &FailedRuleNode,
    judgment_file: &str,
    premise_line: u32,
) -> Option<FailedRuleNode> {
    // The blamed premise itself: keep it and its full subtree.
    if r.line == premise_line && paths_overlap(&r.file, judgment_file) {
        return Some(r.clone());
    }
    // Otherwise keep this rule only if its sub-judgment reaches the premise.
    let child = r
        .child
        .as_ref()
        .and_then(|c| prune_failed(c, judgment_file, premise_line))?;
    Some(FailedRuleNode {
        rule: r.rule.clone(),
        file: r.file.clone(),
        line: r.line,
        cause: r.cause.clone(),
        child: Some(Box::new(child)),
    })
}

/// The file name (final path component) of `path`, for compact tree labels.
fn short_file(path: &str) -> &str {
    path.rsplit('/').next().unwrap_or(path)
}

/// Extract the source of the test function enclosing `file:line`, dedented.
/// `None` if the file can't be read or no enclosing `fn` is found.
///
/// Heuristic: scan up from `line` for the nearest `fn` header (with any leading
/// `#[..]` attributes), then down for the closing brace at the same
/// indentation. This relies on the rustfmt convention that a function's closing
/// brace sits at the function's own indentation, which holds for the test
/// functions we record.
fn extract_test_source(root: &Path, file: &str, line: u32) -> Option<String> {
    let text = std::fs::read_to_string(root.join(file)).ok()?;
    let lines: Vec<&str> = text.lines().collect();
    if line == 0 || line as usize > lines.len() {
        return None;
    }
    let target = line as usize - 1;

    let is_fn = |l: &str| {
        let t = l.trim_start();
        [
            "fn ",
            "pub fn ",
            "pub(crate) fn ",
            "async fn ",
            "pub async fn ",
        ]
        .iter()
        .any(|p| t.starts_with(p))
    };
    let fn_idx = (0..=target).rev().find(|&i| is_fn(lines[i]))?;
    let indent = lines[fn_idx].len() - lines[fn_idx].trim_start().len();

    // Include any attribute lines (e.g. `#[test]`) directly above the `fn`.
    let mut start = fn_idx;
    while start > 0 && lines[start - 1].trim_start().starts_with("#[") {
        start -= 1;
    }

    let close = format!("{}}}", " ".repeat(indent));
    let end = (fn_idx + 1..lines.len()).find(|&i| lines[i] == close)?;

    let block: Vec<String> = lines[start..=end]
        .iter()
        .map(|l| {
            if l.len() >= indent {
                l[indent..].to_string()
            } else {
                l.trim_start().to_string()
            }
        })
        .collect();
    Some(block.join("\n"))
}

/// Positive-coverage cell for a rule in the index table: `✗` if no test
/// exercised it, otherwise a `[N tests]` markdown link to a detail page.
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

/// Collapse a premise's source text to a single line and escape `|` so it
/// can sit in a markdown table cell.
fn premise_label(raw: &str) -> String {
    raw.split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
        .replace('|', "\\|")
}

/// Escape the HTML metacharacters in `s` so source text can sit inside the
/// raw-HTML elements the code view emits.
fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
}

/// Write the index and per-judgment subpages into `out_dir`. Standalone output
/// is viewed as markdown, so detail-page links use the `.md` extension.
pub fn write_all(
    out_dir: &Path,
    judgments: &[Judgment],
    cov: &Coverage,
    github_base: Option<&str>,
    source_root: Option<&Path>,
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
        let body = render_subpage(j, cov, "md");
        std::fs::write(out_dir.join(format!("{}.md", slug)), body)?;

        for page in render_detail_pages_for(j, cov, github_base, source_root) {
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
/// by `positive_num` / `positive_cell`.
fn pos_detail_slug(judgment: &str, rule: &str) -> String {
    format!("{}__{}__pos", slug(judgment), anchor(rule))
}

/// Filename stem for a premise's negative detail page. Must match the link
/// emitted by `negative_num`.
fn neg_detail_slug(judgment: &str, rule: &str, premise_line: u32) -> String {
    format!("{}__{}__p{premise_line}__neg", slug(judgment), anchor(rule))
}
