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
//!
//! The same data is also rendered the other way round: [`render_by_test_index`]
//! and [`render_test_pages`] give a view organized by *test*, where each test
//! shows the rules it proves and the premises it fails on. The two views
//! cross-link, so a reader can go from a premise to the tests that exercise it
//! and from a test back to everything else that exercises the same premise.

use crate::jsonl::{paths_overlap, Coverage, PremiseLoc, TestCoverage, TestLoc};
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
table.cov-code tr.cov-current td{background:rgba(31,120,255,.28)}\n\
ul.cov-tree,ul.cov-tree ul{list-style:none;margin:0;padding-left:1.2em;font-family:var(--mono-font,monospace)}\n\
ul.cov-tree{font-size:.85em}\n\
ul.cov-tree ul{font-size:1em}\n\
ul.cov-tree code{font-size:inherit}\n\
ul.cov-tree summary{cursor:pointer}\n\
.cov-tree-loc{color:#888}\n\
.cov-tree-fail{color:#b35900}\n\
.cov-tree-scroll{overflow-x:auto}\n\
ul.cov-tree li,ul.cov-tree summary{white-space:nowrap}\n\
</style>\n";

/// Filename stem of the page explaining how to read the report. Every generated
/// page links to it (see [`how_to_read_footer`]).
pub const HOW_TO_READ_SLUG: &str = "coverage-how-to-read";

/// The footer every coverage page ends with. Written as a markdown link so
/// mdbook rewrites the `.md` to `.html` itself. The leading blank line matters:
/// without it the `---` would turn the last line of the page into a heading.
fn how_to_read_footer() -> String {
    format!("\n\n---\n\n[How to read this page](./{HOW_TO_READ_SLUG}.md)\n")
}

/// Render the standing explanation of the report's notation, which every page
/// links to in its footer.
pub fn render_how_to_read() -> String {
    let mut s = String::new();
    s.push_str("# How to read the coverage report\n\n");
    s.push_str(
        "The [coverage report](./coverage.md) lists every `judgment_fn!` in the model and every \
         inference rule inside it. The numbers come from the test suite: they are recorded while \
         `cargo test` runs, so they say what the tests actually exercised, not what the model can \
         in principle prove.\n\n",
    );

    s.push_str("## The index table\n\n");
    s.push_str(
        "The report's front page has one row per judgment and one row per rule inside it. The \
         positive column says how many tests proved something with that rule. The negative \
         column is a fraction: how many of the rule's premises that *can* fail have been seen \
         to fail, out of how many there are.\n\n",
    );

    s.push_str("## Positive and negative coverage\n\n");
    s.push_str(
        "A judgment's own page shows each rule as its source, one line per premise with the \
         conclusion below the `---` separator, and puts a number on each line:\n\n",
    );
    s.push_str(
        "- The **green** number on a rule's conclusion is *positive* coverage: how many tests \
         proved something using that rule. It says the rule can fire.\n",
    );
    s.push_str(
        "- The **orange** number on a premise is *negative* coverage: how many tests failed \
         while trying to prove that premise. It says the premise is load-bearing, because some \
         program was rejected by it.\n\n",
    );
    s.push_str(
        "A rule with positive coverage but no negative coverage on any premise is only tested \
         in one direction: the tests show it accepting programs, but never show a premise doing \
         the work of rejecting one.\n\n",
    );

    s.push_str("## The other cells\n\n");
    s.push_str(
        "- **✗** means nothing was observed. For a conclusion, no test proved anything with the \
         rule; for a premise, no test failed on it. This is a gap in the test suite, not \
         necessarily a bug in the model.\n",
    );
    s.push_str(
        "- **N/A** marks a premise that cannot fail once the rule's conclusion matches (a `let` \
         without a `?`), so there is nothing to cover negatively. On the index it marks a rule \
         with no fallible premise at all.\n",
    );
    s.push_str(
        "- **no applicable rule observed** on a judgment means some test called the judgment and \
         none of its rules matched at all.\n\n",
    );

    s.push_str("## Following a number\n\n");
    s.push_str(
        "Clicking a number opens that cell's page: the tests behind the number, each with its \
         source and the proof tree it produced (successful trees for positive coverage, the \
         failed tree pruned to the stacks involving the premise for negative coverage). The \
         chart at the top of that page is the same rule, with the cell you came from \
         highlighted, so you can move to a neighbouring cell without going back.\n\n",
    );
    s.push_str(
        "The [by-test view](./coverage-by-test.md) is the same data organized the other way \
         round: one page per test, listing the rules it proves and the premises it fails on. \
         Every row links back, so \"what else does this test cover\" and \"what else covers this \
         premise\" are both one click away.\n",
    );
    s
}

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
    s.push_str(&how_to_read_footer());
    s
}

/// Render one subpage per judgment as a code view. `link_ext` is the extension
/// used for the links from coverage numbers to detail pages: `"md"` for the
/// standalone CLI report (viewed as markdown) and `"html"` for the mdbook
/// preprocessor (mdbook only rewrites `.md`→`.html` for markdown-syntax links,
/// not for the raw-HTML `<a>` we emit here).
pub fn render_subpage(
    j: &Judgment,
    cov: &Coverage,
    link_ext: &str,
    github_base: Option<&str>,
) -> String {
    let mut s = String::new();
    s.push_str(&format!("# Judgment `{}`\n\n", j.name));
    if !j.source_extract.is_empty() {
        // A judgment's doc comment can itself contain a fenced code block, so
        // the fence has to outrun the longest backtick run inside it.
        let fence = "`".repeat(fence_len(&j.source_extract));
        s.push_str(&format!(
            "{fence}rust,ignore\n{}\n{fence}\n\n",
            j.source_extract
        ));
    }
    s.push_str(&match github_base {
        Some(base) => format!(
            "[Source: `{file}:{line}`]({base}/{file}#L{line})\n\n",
            file = j.file,
            line = j.line,
        ),
        None => format!("Source: `{}:{}`\n\n", j.file, j.line),
    });
    if cov.no_applicable_rule_observed(&j.file, &j.name) {
        s.push_str("_No applicable rule observed: at least one test exercised this judgment with no matching rule._\n\n");
    }
    if j.rules.is_empty() {
        s.push_str("_No rules discovered._\n");
        s.push_str(&how_to_read_footer());
        return s;
    }

    s.push_str(
        "The number on each rule's conclusion is **positive** coverage; the number on each \
         premise is **negative** coverage. Click a number to browse the tests.\n\n",
    );
    s.push_str(STYLE);
    s.push('\n');
    for r in &j.rules {
        s.push_str(&render_rule_block(j, r, cov, link_ext, Highlight::None));
        s.push('\n');
    }
    s.push_str(&how_to_read_footer());
    s
}

/// Length of a code fence that safely encloses `code`: longer than the longest
/// run of backticks in it, and never shorter than the usual three.
fn fence_len(code: &str) -> usize {
    let longest_run = code
        .split(|c| c != '`')
        .map(|run| run.len())
        .max()
        .unwrap_or(0);
    (longest_run + 1).max(3)
}

/// Which row of a rule's coverage chart to highlight as the "current" cell when
/// the chart is embedded at the top of a detail page (see [`chart_section`]).
#[derive(Clone, Copy)]
enum Highlight {
    /// No row highlighted (the chart as rendered on a judgment subpage).
    None,
    /// Highlight the premise on this source line (a negative detail page).
    Premise(u32),
    /// Highlight the conclusion (a positive detail page).
    Conclusion,
}

/// Render one rule as a three-column code view: line number, coverage count,
/// source line. Premises (above the separator) carry their negative coverage;
/// the conclusion (below it) carries the rule's positive coverage. Emitted as a
/// single blank-line-free HTML block (see [`STYLE`]).
fn render_rule_block(
    j: &Judgment,
    r: &Rule,
    cov: &Coverage,
    link_ext: &str,
    highlight: Highlight,
) -> String {
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
        let cur = matches!(highlight, Highlight::Premise(l) if l == p.line);
        s.push_str(&format!(
            "<tr{cls}><td class=\"cov-ln\">{ln}</td><td class=\"cov-num neg\">{num}</td>\
             <td class=\"cov-src-line\">{code}</td></tr>\n",
            cls = if cur { " class=\"cov-current\"" } else { "" },
            ln = p.line,
            num = negative_num(cov, j, r, p, link_ext),
            code = src_cell(&format!("({})", p.raw_text)),
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
    let concl_cls = if matches!(highlight, Highlight::Conclusion) {
        "cov-concl cov-current"
    } else {
        "cov-concl"
    };
    s.push_str(&format!(
        "<tr class=\"{cls}\"><td class=\"cov-ln\">{ln}</td><td class=\"cov-num pos\">{num}</td>\
         <td class=\"cov-src-line\">{code}</td></tr>\n",
        cls = concl_cls,
        ln = r.line + 1,
        num = positive_num(cov, j, r, link_ext),
        code = src_cell(&concl),
    ));

    s.push_str("</table>\n</div>\n");
    s
}

/// Escape source text for a `.cov-src-line` cell: HTML-escape, then turn
/// newlines into `<br>`. A multi-line premise or conclusion (e.g. an inline
/// `match`) otherwise puts a blank line inside the raw-HTML block, which
/// terminates it under CommonMark and leaks the rest of the table as literal
/// markdown. `<br>` (with the cell's `white-space:pre-wrap`) keeps the line
/// breaks and indentation without any physical newline in the emitted HTML.
fn src_cell(s: &str) -> String {
    html_escape(s).replace('\n', "<br>")
}

/// The rule's coverage chart (see [`render_rule_block`]) wrapped with the shared
/// stylesheet, for embedding at the top of a detail page. `highlight` marks the
/// cell this page is about; the other cells stay clickable so the reader can
/// hop to a sibling cell's coverage.
fn chart_section(
    j: &Judgment,
    r: &Rule,
    cov: &Coverage,
    link_ext: &str,
    highlight: Highlight,
) -> String {
    let mut s = STYLE.to_string();
    s.push('\n');
    s.push_str(&render_rule_block(j, r, cov, link_ext, highlight));
    s.push('\n');
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
/// is the markdown body. `args_json` is the page's sidecar argument data (see
/// [`ArgSink`]), written to `coverage-args/{slug}.args.json` and fetched lazily
/// by the page's inline script; empty when the page has no arguments to show.
pub struct DetailPage {
    pub slug: String,
    pub title: String,
    pub content: String,
    pub args_json: String,
}

/// Subdirectory (relative to the pages) holding the sidecar `args` JSON files a
/// page's inline script fetches. Both the standalone report and the mdbook
/// preprocessor place the JSON here so the pages' `./coverage-args/…` fetch URL
/// resolves the same way in each.
pub const ARGS_SUBDIR: &str = "coverage-args";

/// Write a detail page's sidecar `args` JSON to `dir/{slug}.args.json`, creating
/// `dir` on first use. No-op when the page has no arguments (so no empty files
/// are written). The JSON is byte-stable for a given page (see [`ArgSink`]).
pub fn write_args_json(dir: &Path, page: &DetailPage) -> Result<()> {
    if page.args_json.is_empty() {
        return Ok(());
    }
    std::fs::create_dir_all(dir).with_context(|| format!("creating {}", dir.display()))?;
    std::fs::write(
        dir.join(format!("{}.args.json", page.slug)),
        &page.args_json,
    )?;
    Ok(())
}

/// Collects the per-node argument data for one detail page while it renders, so
/// the (potentially large) `Debug` values can ship as a sidecar JSON file
/// fetched on demand instead of inlined into the page HTML. Each node that shows
/// arguments is assigned a stable id (`n0`, `n1`, …) emitted as its
/// `data-arg-id`; the page's inline script ([`args_script`]) fetches
/// `{slug}.args.json` and hydrates the matching disclosure only when it is
/// opened. Keeping the values out of the HTML keeps the initial page load and
/// the mdbook search index small.
#[derive(Default)]
struct ArgSink {
    /// `(id, rows)` where each row is `[name, value]` (an empty name renders the
    /// value alone, as a failed judgment's argument string does).
    entries: Vec<(String, Vec<[String; 2]>)>,
}

impl ArgSink {
    fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Register a node's argument rows and return its `data-arg-id`.
    fn push(&mut self, rows: Vec<[String; 2]>) -> String {
        let id = format!("n{}", self.entries.len());
        self.entries.push((id.clone(), rows));
        id
    }

    /// Serialize to the sidecar object `{ "n0": [["name","value"], …], … }`, or
    /// `""` when empty so callers can skip writing a file. Keys are sorted (via
    /// `serde_json::Map`), so identical input yields byte-identical output — this
    /// keeps `mdbook serve` from rebuilding in a loop when the preprocessor
    /// rewrites the file each build.
    fn to_json(&self) -> String {
        if self.entries.is_empty() {
            return String::new();
        }
        let map: serde_json::Map<String, serde_json::Value> = self
            .entries
            .iter()
            .map(|(id, rows)| {
                let arr = rows
                    .iter()
                    .map(|[n, v]| serde_json::json!([n, v]))
                    .collect();
                (id.clone(), serde_json::Value::Array(arr))
            })
            .collect();
        serde_json::to_string(&serde_json::Value::Object(map)).unwrap_or_default()
    }
}

/// Inline script injected once per detail page. On the first opening of an
/// `args` disclosure it fetches the page's `{slug}.args.json` (cached for the
/// page), renders that node's rows, and frees them again when the disclosure is
/// closed — so the argument values are downloaded and materialized only while a
/// reader is actually looking at them. `slug` is ASCII alphanumeric/`_` (see
/// [`slug`]/[`anchor`]), so it needs no escaping in the URL. Emitted as one
/// `<script>` block; its raw text is untrusted only in the JSON it fetches,
/// which is inserted via `textContent`, never `innerHTML`.
fn args_script(slug: &str) -> String {
    ARGS_SCRIPT.replace("__ARGS_URL__", &format!("./coverage-args/{slug}.args.json"))
}

const ARGS_SCRIPT: &str = r#"<script>
(function () {
  var url = "__ARGS_URL__";
  var cache;
  function load() { return cache || (cache = fetch(url).then(function (r) { return r.json(); })); }
  function fill(d) {
    var ul = d.querySelector("ul");
    load().then(function (m) {
      var rows = m[d.getAttribute("data-arg-id")] || [];
      ul.textContent = "";
      rows.forEach(function (p) {
        var li = document.createElement("li");
        var c = document.createElement("code");
        c.textContent = p[0] ? p[0] + " = " + p[1] : p[1];
        li.appendChild(c);
        ul.appendChild(li);
      });
    });
  }
  document.addEventListener("DOMContentLoaded", function () {
    var list = document.querySelectorAll("details.cov-args[data-arg-id]");
    for (var i = 0; i < list.length; i++) {
      (function (d) {
        d.addEventListener("toggle", function () {
          var ul = d.querySelector("ul");
          if (d.open) { fill(d); } else { ul.textContent = ""; }
        });
      })(list[i]);
    }
  });
})();
</script>
"#;

/// Build the detail pages for one judgment: one per covered rule (positive) and
/// one per fallible premise that was negatively tested. Cells produced by
/// `positive_num` / `negative_num` link to exactly these pages. Each page opens
/// with the rule's coverage chart (the current cell highlighted, so the reader
/// can jump to sibling cells) and then lists the tests with, for each, its
/// source location and (when `source_root` is set and the file is readable) the
/// test function's source inline. `link_ext` is the extension the embedded
/// chart's cell links use (`"md"` for the CLI report, `"html"` for mdbook), as
/// for [`render_subpage`].
pub fn render_detail_pages_for(
    j: &Judgment,
    cov: &Coverage,
    github_base: Option<&str>,
    source_root: Option<&Path>,
    link_ext: &str,
) -> Vec<DetailPage> {
    let mut pages = Vec::new();
    for r in &j.rules {
        // Positive: every test that exercised this rule.
        if let Some(locs) = cov.positive_tests(&j.name, &r.name) {
            if !locs.is_empty() {
                let slug = pos_detail_slug(&j.name, &r.name);
                let mut content = format!("# Positive coverage: `{}` / `{}`\n\n", j.name, r.name);
                content.push_str(&chart_section(j, r, cov, link_ext, Highlight::Conclusion));
                content.push_str(&format!(
                    "{} {} exercised this rule:\n\n",
                    locs.len(),
                    plural(locs.len()),
                ));
                let mut sink = ArgSink::default();
                content.push_str(&test_list(
                    github_base,
                    source_root,
                    locs,
                    TreeSection::Positive(cov),
                    &mut sink,
                ));
                if !sink.is_empty() {
                    content.push_str(&args_script(&slug));
                }
                content.push_str(&how_to_read_footer());
                pages.push(DetailPage {
                    title: format!("{} / {} (positive)", j.name, r.name),
                    content,
                    args_json: sink.to_json(),
                    slug,
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
            content.push_str("\n\n");
            content.push_str(&chart_section(
                j,
                r,
                cov,
                link_ext,
                Highlight::Premise(p.line),
            ));
            content.push_str(&format!(
                "{} {} failed proving this premise:\n\n",
                tests.len(),
                plural(tests.len()),
            ));
            let slug = neg_detail_slug(&j.name, &r.name, p.line);
            let mut sink = ArgSink::default();
            content.push_str(&test_list(
                github_base,
                source_root,
                &tests,
                TreeSection::Negative {
                    cov,
                    judgment_file: &j.file,
                    premise_line: p.line,
                },
                &mut sink,
            ));
            if !sink.is_empty() {
                content.push_str(&args_script(&slug));
            }
            content.push_str(&how_to_read_footer());
            pages.push(DetailPage {
                title: format!("{} / {} premise@{} (negative)", j.name, r.name, p.line),
                content,
                args_json: sink.to_json(),
                slug,
            });
        }
    }
    pages
}

/// Stylesheet for the by-test pages' tables. Their cells hold source text with
/// long unbroken tokens (paths, judgment calls), which the default table layout
/// cannot shrink, so the last column runs off the page; allowing a break
/// anywhere inside those cells keeps every column on screen. Scoped to `main`
/// so it beats mdbook's bare `table` rules, and emitted with no blank lines for
/// the reason given on [`STYLE`].
const BY_TEST_STYLE: &str = "<style>\n\
main table{width:100%;display:table}\n\
main table code{overflow-wrap:anywhere;white-space:normal}\n\
</style>\n";

/// Render the index of the by-test view: every test that recorded coverage,
/// grouped by source file, linking to its own page (see [`render_test_pages`]).
pub fn render_by_test_index(cov: &Coverage, source_root: Option<&Path>) -> String {
    let mut s = String::new();
    s.push_str("# Coverage by test\n\n");
    s.push_str(
        "The same data as the [coverage report](./coverage.md), organized by test rather than \
         by judgment: each test lists the rules it proves and the premises it is observed to \
         fail on.\n",
    );
    let by_test = cov.by_test();
    if by_test.is_empty() {
        // Blank line first, or this joins the paragraph above into one run of
        // text. This is the branch a book built without running the tests takes.
        s.push_str("\n_No coverage recorded._\n");
        s.push_str(&how_to_read_footer());
        return s;
    }
    let mut current_file = "";
    for (loc, tc) in &by_test {
        if loc.file != current_file {
            current_file = &loc.file;
            s.push_str(&format!("\n## `{current_file}`\n\n"));
            s.push_str("| Test | Rules proved | Premises failed |\n| --- | --- | --- |\n");
        }
        s.push_str(&format!(
            "| [{label}](./{slug}.md) | {rules} | {premises} |\n",
            label = test_label(source_root, loc),
            slug = test_page_slug(loc),
            rules = tc.rules.len(),
            premises = tc.premises.len(),
        ));
    }
    s.push_str(&how_to_read_footer());
    s
}

/// Build one page per test: the test's source, the rules it proves, the premises
/// it fails on, and its proof tree(s). Every rule and premise row links to that
/// cell's detail page in the judgment-organized view, which is where the reader
/// finds the *other* tests covering the same cell.
///
/// Unlike the per-cell detail pages, which cap how many trees they inline (see
/// [`MAX_TREES_PER_CELL`]), a test page carries exactly one test's trees, so
/// every recorded tree is reachable from here.
pub fn render_test_pages(
    judgments: &[Judgment],
    cov: &Coverage,
    github_base: Option<&str>,
    source_root: Option<&Path>,
) -> Vec<DetailPage> {
    cov.by_test()
        .iter()
        .map(|(loc, tc)| render_test_page(judgments, cov, loc, tc, github_base, source_root))
        .collect()
}

fn render_test_page(
    judgments: &[Judgment],
    cov: &Coverage,
    loc: &TestLoc,
    tc: &TestCoverage,
    github_base: Option<&str>,
    source_root: Option<&Path>,
) -> DetailPage {
    let slug = test_page_slug(loc);
    let label = format!("{}:{}", loc.file, loc.line);
    // The heading's function name and the source block below come from the same
    // block, so read it once.
    let src = source_root.and_then(|root| extract_test_source(root, &loc.file, loc.line));
    let name = src.as_deref().and_then(test_fn_name);
    let heading = name.clone().unwrap_or_else(|| label.clone());

    // [`STYLE`] as well as [`BY_TEST_STYLE`]: the proof trees below are rendered
    // by the same helpers the cell pages use, and their `cov-tree` rules live
    // only in `STYLE`. Emitting it whole (rather than the tree rules alone)
    // keeps this page in step if those rules ever change; the chart rules it
    // also carries are inert on a page with no chart.
    let mut content = format!("# Test `{heading}`\n\n{STYLE}\n{BY_TEST_STYLE}\n");
    match github_base {
        Some(base) => content.push_str(&format!(
            "**Source location:** [{label}]({base}/{file}#L{line})\n\n",
            file = loc.file,
            line = loc.line,
        )),
        None => content.push_str(&format!("**Source location:** {label}\n\n")),
    }
    if let Some(src) = &src {
        content.push_str("```rust,ignore\n");
        content.push_str(src);
        content.push_str("\n```\n\n");
    }

    content.push_str(&format!("## Rules proved ({})\n\n", tc.rules.len()));
    if tc.rules.is_empty() {
        content.push_str("_This test records no positive coverage._\n\n");
    } else {
        content.push_str("| Judgment | Rule | All tests of this rule |\n| --- | --- | --- |\n");
        for cr in &tc.rules {
            let scraped = judgments
                .iter()
                .find(|j| j.name == cr.judgment)
                .and_then(|j| j.rules.iter().find(|r| r.name == cr.rule).map(|r| (j, r)));
            match scraped {
                Some((j, r)) => content.push_str(&format!(
                    "| {judgment} | {rule} | {tests} |\n",
                    judgment = judgment_link(j),
                    rule = rule_link(j, r),
                    tests = positive_cell(cov, &cr.judgment, &cr.rule),
                )),
                // A rule the tests exercised but the scraper does not find, so
                // it has no page to link to. Both hand-rolled `ProofTree`s
                // (which are not `judgment_fn!` rules at all) and judgments the
                // scraper fails to parse land here, so the count is still worth
                // showing.
                None => {
                    let n = cov
                        .positive_tests(&cr.judgment, &cr.rule)
                        .map_or(0, |t| t.len());
                    content.push_str(&format!(
                        "| `{judgment}` | `{rule}` | {n} {tests} (not in the report) |\n",
                        judgment = cr.judgment,
                        rule = cr.rule,
                        tests = plural(n),
                    ));
                }
            }
        }
        content.push('\n');
    }

    content.push_str(&format!("## Premises failed ({})\n\n", tc.premises.len()));
    if tc.premises.is_empty() {
        content.push_str("_This test records no negative coverage._\n\n");
    } else {
        content.push_str(
            "The last column links to the premise's page when the judgment view counts this \
             failure against that premise. It does not when the premise is read as infallible, \
             or when the failure was blamed inside a multi-line premise that carries no record \
             on its own first line.\n\n",
        );
        content.push_str(
            "| Judgment | Rule | Premise | All tests of this premise |\n| --- | --- | --- | --- |\n",
        );
        for ploc in &tc.premises {
            match resolve_premise(judgments, ploc) {
                Some((j, r, p)) => {
                    // The judgment view writes a premise's page under exactly
                    // these conditions (see `render_detail_pages_for`), so they
                    // are also what decides whether there is a page to link to.
                    let tests = if p.fallible {
                        cov.negative_premise_tests(&j.file, p.line)
                    } else {
                        Default::default()
                    };
                    let all_tests = if tests.is_empty() {
                        "not counted by the judgment view".to_string()
                    } else {
                        format!(
                            "[{n} {tests}](./{slug}.md)",
                            n = tests.len(),
                            tests = plural(tests.len()),
                            slug = neg_detail_slug(&j.name, &r.name, p.line),
                        )
                    };
                    // Name the blamed line whenever it is not the premise's own
                    // first line, so a reader can see where the span match in
                    // `resolve_premise` put this row and check it against the
                    // source.
                    let at = if ploc.line == p.line {
                        format!("line {}", p.line)
                    } else {
                        format!("line {}, blamed at line {}", p.line, ploc.line)
                    };
                    content.push_str(&format!(
                        "| {judgment} | {rule} | `{premise}` ({at}) | {all_tests} |\n",
                        judgment = judgment_link(j),
                        rule = rule_link(j, r),
                        premise = premise_cell(&p.raw_text),
                    ));
                }
                // A blamed location inside no scraped premise at all: the
                // judgment it names is one the scraper does not find.
                None => content.push_str(&format!(
                    "| - | - | `{}:{}` | not in the report |\n",
                    ploc.file, ploc.line
                )),
            }
        }
        content.push('\n');
    }

    let mut sink = ArgSink::default();
    let trees = format!(
        "{}{}",
        proof_tree_details(
            cov.positive_trees_for(loc),
            github_base,
            source_root,
            &mut sink,
        ),
        failed_tree_details(
            cov.negative_trees_for(loc),
            github_base,
            source_root,
            &mut sink,
        ),
    );
    if !trees.is_empty() {
        content.push_str("## Proof trees\n\n");
        content.push_str(&trees);
    }
    if !sink.is_empty() {
        content.push_str(&args_script(&slug));
    }
    content.push_str(&how_to_read_footer());

    DetailPage {
        title: match name {
            Some(name) => format!("{name} ({}:{})", short_file(&loc.file), loc.line),
            None => format!("{}:{}", short_file(&loc.file), loc.line),
        },
        content,
        args_json: sink.to_json(),
        slug,
    }
}

/// Filename stem for a test's page in the by-test view. Must match the links
/// emitted by [`render_by_test_index`] and by the back-link in [`test_list`].
pub fn test_page_slug(loc: &TestLoc) -> String {
    format!("test__{}__{}", slug(&loc.file), loc.line)
}

/// Markdown link to a judgment's subpage in the judgment-organized view.
fn judgment_link(j: &Judgment) -> String {
    format!("[{name}](./{slug}.md)", name = j.name, slug = slug(&j.name))
}

/// Markdown link to one rule's chart on its judgment's subpage.
fn rule_link(j: &Judgment, r: &Rule) -> String {
    format!(
        "[{rule}](./{slug}.md#{anchor})",
        rule = r.name,
        slug = slug(&j.name),
        anchor = anchor(&r.name),
    )
}

/// Locate the scraped premise a recorded negative-coverage location refers to:
/// the premise whose source lines contain `loc`, in a file whose path overlaps
/// (the record's path and the scraped path have different roots).
///
/// Matching is by line *span*, not by start line as
/// [`Coverage::negative_premise_tests`] does. The macro respans a failure onto
/// the exact sub-premise that failed, while the scraper reads a `for_all` block
/// as a single premise, so most records inside such a block name a line the
/// judgment view can attribute to no premise at all. Spans put those rows under
/// the premise they belong to; whether the judgment view *counts* them is a
/// separate question the caller answers with `negative_premise_tests`.
fn resolve_premise<'a>(
    judgments: &'a [Judgment],
    loc: &PremiseLoc,
) -> Option<(&'a Judgment, &'a Rule, &'a Premise)> {
    judgments.iter().find_map(|j| {
        if !paths_overlap(&loc.file, &j.file) {
            return None;
        }
        j.rules.iter().find_map(|r| {
            r.premises
                .iter()
                .find(|p| premise_spans_line(p, loc.line))
                .map(|p| (j, r, p))
        })
    })
}

/// Whether `line` falls within the source lines `p` occupies. `Premise::line` is
/// its first line and `raw_text` holds its full (possibly multi-line) source.
fn premise_spans_line(p: &Premise, line: u32) -> bool {
    let height = p.raw_text.lines().count().max(1) as u32;
    (p.line..p.line + height).contains(&line)
}

/// The name of the test function whose source is `src`, as [`extract_test_source`]
/// returns it. Taking the block rather than the location keeps the name and the
/// rendered source in agreement, and lets a caller that needs both read the file
/// once.
fn test_fn_name(src: &str) -> Option<String> {
    // The block is dedented and starts with the test's attributes, so the first
    // non-attribute line carrying `fn ` is the function header.
    let header = src
        .lines()
        .find(|l| !l.starts_with("#[") && l.contains("fn "))?;
    let name: String = header
        .split("fn ")
        .nth(1)?
        .chars()
        .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
        .collect();
    (!name.is_empty()).then_some(name)
}

/// How a test is labelled in the by-test index: its function name when we can
/// read the source, and always its line, which is what makes the row unique.
fn test_label(source_root: Option<&Path>, loc: &TestLoc) -> String {
    let src = source_root.and_then(|root| extract_test_source(root, &loc.file, loc.line));
    match src.as_deref().and_then(test_fn_name) {
        Some(name) => format!("{name} (line {})", loc.line),
        None => format!("line {}", loc.line),
    }
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
    sink: &mut ArgSink,
) -> String {
    let tests: Vec<&TestLoc> = tests.into_iter().collect();
    let mut s = String::new();
    for (i, loc) in tests.iter().enumerate() {
        let label = format!("{}:{}", loc.file, loc.line);
        // Every location listed here comes from the coverage data, so the test's
        // page in the by-test view always exists.
        let back = format!(
            " ([all coverage from this test](./{}.md))",
            test_page_slug(loc),
        );
        s.push_str("---\n\n");
        match github_base {
            Some(base) => s.push_str(&format!(
                "**Source location:** [{label}]({base}/{file}#L{line}){back}\n\n",
                label = label,
                base = base,
                file = loc.file,
                line = loc.line,
            )),
            None => s.push_str(&format!("**Source location:** {label}{back}\n\n")),
        }
        if let Some(root) = source_root {
            if let Some(src) = extract_test_source(root, &loc.file, loc.line) {
                s.push_str("```rust,ignore\n");
                s.push_str(&src);
                s.push_str("\n```\n\n");
            }
        }
        if i < MAX_TREES_PER_CELL {
            s.push_str(&tree_details(&trees, loc, github_base, source_root, sink));
        } else if i == MAX_TREES_PER_CELL {
            s.push_str(&format!(
                "_Proof trees omitted for the remaining {} tests; each one is on its test's page \
                 in [Coverage by test](./coverage-by-test.md)._\n\n",
                tests.len() - MAX_TREES_PER_CELL,
            ));
        }
    }
    s
}

/// The collapsible proof-tree disclosure for one test, or an empty string when
/// no tree was recorded (or, on a negative page, none of the test's failed
/// stacks involve the premise this page is about). `github_base`/`source_root`
/// turn each node's `file:line` into a GitHub link carrying a hover preview of
/// the surrounding source.
fn tree_details(
    trees: &TreeSection<'_>,
    loc: &TestLoc,
    github_base: Option<&str>,
    source_root: Option<&Path>,
    sink: &mut ArgSink,
) -> String {
    match trees {
        TreeSection::Positive(cov) => {
            proof_tree_details(cov.positive_trees_for(loc), github_base, source_root, sink)
        }
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
            failed_tree_details(&pruned, github_base, source_root, sink)
        }
    }
}

/// Wrap a pre-rendered HTML tree body in a collapsed `<details>` disclosure. The
/// body sits in a horizontally scrollable box (`cov-tree-scroll`) because nodes
/// render on a single line each (no wrapping) and a deep proof stack's growing
/// indentation would otherwise overflow the page; scrolling keeps every line
/// readable at any depth. Emitted as one raw-HTML block with no internal blank
/// lines: a blank line would close the HTML block under CommonMark and leak the
/// rest as literal markdown. `body_html` is already HTML (its text content
/// escaped by the node renderers), so it is embedded verbatim.
fn tree_disclosure(summary: &str, body_html: &str) -> String {
    format!(
        "<details>\n<summary>{summary}</summary>\n<div class=\"cov-tree-scroll\">\n{body}</div>\n</details>\n\n",
        summary = summary,
        body = body_html,
    )
}

/// Nesting depth at which tree nodes start collapsed. The top `TREE_OPEN_DEPTH`
/// levels render expanded so the high-level proof path is visible at a glance;
/// nodes exactly at this depth start collapsed, since proof stacks routinely run
/// dozens of levels deep and would otherwise make the default view unwieldy.
/// Everything below the fold is emitted `open`, so expanding one collapsed node
/// reveals its whole subtree in a single click.
const TREE_OPEN_DEPTH: usize = 4;

/// The opening `<details>` tag for a node at `depth`: expanded except at the
/// fold depth itself (see [`TREE_OPEN_DEPTH`]).
fn details_tag(depth: usize) -> &'static str {
    if depth == TREE_OPEN_DEPTH {
        "<details>"
    } else {
        "<details open>"
    }
}

/// Largest proof tree we render inline. Where-clause solving recurses deeply,
/// so a single success tree can reach thousands of nodes, which is neither
/// readable nor cheap to ship in the search index. We render the first
/// [`MAX_TREE_NODES`] (depth-first) and note how many were elided.
const MAX_TREE_NODES: usize = 200;

/// The success proof tree(s) for one test as a nested collapsible list, or `""`
/// if none were recorded.
fn proof_tree_details(
    nodes: &[ProofTreeNode],
    github_base: Option<&str>,
    source_root: Option<&Path>,
    sink: &mut ArgSink,
) -> String {
    if nodes.is_empty() {
        return String::new();
    }
    let total: usize = nodes.iter().map(count_proof_nodes).sum();
    let mut body = String::from("<ul class=\"cov-tree\">\n");
    let mut budget = MAX_TREE_NODES;
    for n in nodes {
        render_proof_node(
            n,
            github_base,
            source_root,
            0,
            &[],
            &mut body,
            &mut budget,
            sink,
        );
    }
    body.push_str("</ul>\n");
    append_truncation_note(&mut body, total);
    tree_disclosure("Proof tree", &body)
}

fn count_proof_nodes(n: &ProofTreeNode) -> usize {
    1 + n.children.iter().map(count_proof_nodes).sum::<usize>()
}

/// Render one success-tree node as an `<li>`. Nodes with children become a
/// collapsible `<details>` (open or collapsed per [`details_tag`]) so any
/// subtree can be folded away; leaves are plain list items.
fn render_proof_node(
    n: &ProofTreeNode,
    github_base: Option<&str>,
    source_root: Option<&Path>,
    depth: usize,
    parent_attrs: &[(String, String)],
    out: &mut String,
    budget: &mut usize,
    sink: &mut ArgSink,
) {
    if *budget == 0 {
        return;
    }
    *budget -= 1;
    let rule = n
        .rule
        .as_deref()
        .map(|r| format!(" ({})", html_escape(r)))
        .unwrap_or_default();
    let label = format!(
        "<code>{judgment}{rule}</code> {loc}",
        judgment = html_escape(&n.judgment),
        rule = rule,
        loc = loc_link(&n.file, n.line, github_base, source_root),
    );
    // Show only arguments that differ from the parent: ambient ones (e.g. the
    // whole program) are constant down a path, so this shows them once near the
    // top instead of on every node.
    let fresh: Vec<(String, String)> = n
        .attributes
        .iter()
        .filter(|(k, v)| !parent_attrs.iter().any(|(pk, pv)| pk == k && pv == v))
        .cloned()
        .collect();
    let attrs = render_attrs(&fresh, sink);
    if n.children.is_empty() && attrs.is_empty() {
        out.push_str(&format!("<li>{label}</li>\n"));
        return;
    }
    out.push_str(&format!(
        "<li>{details}<summary>{label}</summary>\n<ul class=\"cov-tree\">\n",
        details = details_tag(depth),
    ));
    out.push_str(&attrs);
    for c in &n.children {
        render_proof_node(
            c,
            github_base,
            source_root,
            depth + 1,
            &n.attributes,
            out,
            budget,
            sink,
        );
    }
    out.push_str("</ul>\n</details></li>\n");
}

/// A collapsed `args` disclosure whose body is filled in lazily by
/// [`args_script`]: the node's `(name, value)` rows are registered in `sink`
/// (shipped as sidecar JSON) and only the empty placeholder is emitted, keyed by
/// the returned `data-arg-id`. Returns `""` when there are no attributes.
fn render_attrs(attributes: &[(String, String)], sink: &mut ArgSink) -> String {
    if attributes.is_empty() {
        return String::new();
    }
    let rows: Vec<[String; 2]> = attributes
        .iter()
        .map(|(name, value)| [name.clone(), value.clone()])
        .collect();
    args_placeholder(sink.push(rows))
}

/// A collapsed `args` disclosure for a failed judgment node, showing the single
/// argument-list string the judgment was applied to (emitted lazily like
/// [`render_attrs`], as one row with an empty name so it renders alone), or `""`
/// when it is empty.
fn render_failed_args(args: &str, sink: &mut ArgSink) -> String {
    if args.is_empty() {
        return String::new();
    }
    args_placeholder(sink.push(vec![[String::new(), args.to_string()]]))
}

/// The empty `args` disclosure emitted in the HTML; [`args_script`] fetches the
/// sidecar JSON and fills the inner `<ul>` for `id` when the reader opens it.
fn args_placeholder(id: String) -> String {
    format!(
        "<li><details class=\"cov-args\" data-arg-id=\"{id}\"><summary><em>args</em></summary>\n\
         <ul class=\"cov-tree\"></ul>\n</details></li>\n",
    )
}

/// The failed proof tree(s) for one test, already pruned to the relevant
/// premise, as a nested collapsible list, or `""` if nothing survived pruning.
fn failed_tree_details(
    nodes: &[FailedTreeNode],
    github_base: Option<&str>,
    source_root: Option<&Path>,
    sink: &mut ArgSink,
) -> String {
    if nodes.is_empty() {
        return String::new();
    }
    let total: usize = nodes.iter().map(count_failed_nodes).sum();
    let mut body = String::from("<ul class=\"cov-tree\">\n");
    let mut budget = MAX_TREE_NODES;
    for n in nodes {
        render_failed_node(
            n,
            github_base,
            source_root,
            0,
            "",
            &mut body,
            &mut budget,
            sink,
        );
    }
    body.push_str("</ul>\n");
    append_truncation_note(&mut body, total);
    tree_disclosure("Failed proof tree", &body)
}

fn count_failed_nodes(j: &FailedTreeNode) -> usize {
    1 + j.rules.iter().map(count_failed_rule_nodes).sum::<usize>()
}

fn count_failed_rule_nodes(r: &FailedRuleNode) -> usize {
    1 + r.child.as_deref().map_or(0, count_failed_nodes)
}

fn render_failed_node(
    j: &FailedTreeNode,
    github_base: Option<&str>,
    source_root: Option<&Path>,
    depth: usize,
    parent_args: &str,
    out: &mut String,
    budget: &mut usize,
    sink: &mut ArgSink,
) {
    if *budget == 0 {
        return;
    }
    *budget -= 1;
    let label = format!(
        "<code>{judgment} failed</code> {loc}",
        judgment = html_escape(&j.judgment),
        loc = loc_link(&j.file, j.line, github_base, source_root),
    );
    // Suppress args identical to the parent judgment's (see `render_proof_node`).
    let attrs = if j.args != parent_args {
        render_failed_args(&j.args, sink)
    } else {
        String::new()
    };
    if j.rules.is_empty() && attrs.is_empty() {
        out.push_str(&format!("<li>{label}</li>\n"));
        return;
    }
    out.push_str(&format!(
        "<li>{details}<summary>{label}</summary>\n<ul class=\"cov-tree\">\n",
        details = details_tag(depth),
    ));
    out.push_str(&attrs);
    for r in &j.rules {
        render_failed_rule(
            r,
            github_base,
            source_root,
            depth + 1,
            &j.args,
            out,
            budget,
            sink,
        );
    }
    out.push_str("</ul>\n</details></li>\n");
}

fn render_failed_rule(
    r: &FailedRuleNode,
    github_base: Option<&str>,
    source_root: Option<&Path>,
    depth: usize,
    parent_args: &str,
    out: &mut String,
    budget: &mut usize,
    sink: &mut ArgSink,
) {
    if *budget == 0 {
        return;
    }
    *budget -= 1;
    let name = match &r.rule {
        Some(name) => format!("rule \"{}\"", html_escape(name)),
        None => "rule".to_string(),
    };
    let loc = loc_link(&r.file, r.line, github_base, source_root);
    match &r.child {
        // A nested judgment failure: recurse to show where it broke.
        Some(child) => {
            out.push_str(&format!(
                "<li>{details}<summary><code>{name}</code> {loc}</summary>\n<ul class=\"cov-tree\">\n",
                details = details_tag(depth),
            ));
            render_failed_node(
                child,
                github_base,
                source_root,
                depth + 1,
                parent_args,
                out,
                budget,
                sink,
            );
            out.push_str("</ul>\n</details></li>\n");
        }
        // A terminal failure: show the cause tag.
        None => out.push_str(&format!(
            "<li><code>{name}</code> {loc} <span class=\"cov-tree-fail\">(failed: {cause})</span></li>\n",
            cause = html_escape(&r.cause),
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

/// Lines of source context (each side) shown in a proof-tree node's hover tooltip.
const SNIPPET_CONTEXT: usize = 3;

/// A source-location reference for a proof-tree node: the file's basename and
/// line, linked to GitHub when `github_base` is set and carrying a `title`
/// tooltip of the surrounding source when `source_root` is set and the file is
/// readable. Falls back to a plain `<span>` (still with the tooltip) when there
/// is no GitHub base.
fn loc_link(
    file: &str,
    line: u32,
    github_base: Option<&str>,
    source_root: Option<&Path>,
) -> String {
    let short = html_escape(short_file(file));
    let title = match source_snippet(source_root, file, line) {
        Some(s) => format!(" title=\"{}\"", attr_escape(&s)),
        None => String::new(),
    };
    match github_base {
        Some(base) => format!(
            "<a class=\"cov-tree-loc\" href=\"{base}/{href}#L{line}\"{title} target=\"_blank\" rel=\"noopener noreferrer\">{short}:{line}</a>",
            href = attr_escape(file),
        ),
        None => format!("<span class=\"cov-tree-loc\"{title}>{short}:{line}</span>"),
    }
}

/// The source lines around `file:line` (1-based), each prefixed with its line
/// number and the target line marked with `>`, for a hover tooltip. `None` when
/// `root` is unset, the file can't be read, or `line` is out of range.
fn source_snippet(root: Option<&Path>, file: &str, line: u32) -> Option<String> {
    let root = root?;
    let text = std::fs::read_to_string(root.join(file)).ok()?;
    let lines: Vec<&str> = text.lines().collect();
    if line == 0 || line as usize > lines.len() {
        return None;
    }
    let target = line as usize - 1;
    let start = target.saturating_sub(SNIPPET_CONTEXT);
    let end = (target + SNIPPET_CONTEXT + 1).min(lines.len());
    let snippet: Vec<String> = (start..end)
        .map(|i| {
            let marker = if i == target { ">" } else { " " };
            format!("{marker}{ln:>4}  {src}", ln = i + 1, src = lines[i])
        })
        .collect();
    Some(snippet.join("\n"))
}

/// Escape `s` for use inside a double-quoted HTML attribute value: the HTML
/// metacharacters plus the `"` that would close the attribute.
fn attr_escape(s: &str) -> String {
    html_escape(s).replace('"', "&quot;")
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
        args: j.args.clone(),
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

/// Longest premise source shown in a by-test table cell. Premises run to many
/// lines (a `for_all` block, an inline `match`), and a cell that wide pushes the
/// rest of the table off the page; the full source is on the premise's own page.
const MAX_PREMISE_CELL: usize = 48;

/// A premise's source for a by-test table cell: collapsed to one line as
/// [`premise_label`] does, and truncated to [`MAX_PREMISE_CELL`].
fn premise_cell(raw: &str) -> String {
    let one_line = raw.split_whitespace().collect::<Vec<_>>().join(" ");
    let truncated = match one_line.char_indices().nth(MAX_PREMISE_CELL) {
        Some((end, _)) => format!("{}…", &one_line[..end]),
        None => one_line,
    };
    premise_label(&truncated)
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
    std::fs::write(
        out_dir.join(format!("{HOW_TO_READ_SLUG}.md")),
        render_how_to_read(),
    )?;

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
        let body = render_subpage(j, cov, "md", github_base);
        std::fs::write(out_dir.join(format!("{}.md", slug)), body)?;

        for page in render_detail_pages_for(j, cov, github_base, source_root, "md") {
            if !seen_slugs.insert(page.slug.clone()) {
                eprintln!(
                    "warning: slug collision for coverage detail page `{}`, it will overwrite a sibling",
                    page.slug,
                );
            }
            write_args_json(&out_dir.join(ARGS_SUBDIR), &page)?;
            std::fs::write(out_dir.join(format!("{}.md", page.slug)), page.content)?;
        }
    }

    // The same coverage organized by test, cross-linked with the pages above.
    std::fs::write(
        out_dir.join("coverage-by-test.md"),
        render_by_test_index(cov, source_root),
    )?;
    for page in render_test_pages(judgments, cov, github_base, source_root) {
        if !seen_slugs.insert(page.slug.clone()) {
            eprintln!(
                "warning: slug collision for coverage test page `{}`, it will overwrite a sibling",
                page.slug,
            );
        }
        write_args_json(&out_dir.join(ARGS_SUBDIR), &page)?;
        std::fs::write(out_dir.join(format!("{}.md", page.slug)), page.content)?;
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
