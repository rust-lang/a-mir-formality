use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use formality_coverage::scrape::{Judgment, Rule, parse_judgment_fns};
use formality_coverage::{jsonl, report};
use mdbook_preprocessor::book::{Book, BookItem, Chapter, SectionNumber};
use mdbook_preprocessor::{Preprocessor, PreprocessorContext};
use regex::Regex;

pub struct JudgmentPreprocessor;

impl Preprocessor for JudgmentPreprocessor {
    fn name(&self) -> &str {
        "judgment"
    }

    fn run(&self, ctx: &PreprocessorContext, mut book: Book) -> anyhow::Result<Book> {
        let root = ctx.root.clone();

        let source_dir: String = ctx
            .config
            .get("preprocessor.judgment.source-dir")
            .ok()
            .flatten()
            .unwrap_or_else(|| "src".to_string());

        let github_base: Option<String> = ctx
            .config
            .get("preprocessor.judgment.github-base")
            .ok()
            .flatten()
            .or_else(|| {
                ctx.config
                    .get::<String>("output.html.git-repository-url")
                    .ok()
                    .flatten()
                    .map(|url| format!("{}/blob/main", url.trim_end_matches('/')))
            })
            .map(|s: String| s.trim_end_matches('/').to_string());

        let src_dir = root.join(&source_dir);
        let link_root = src_dir
            .canonicalize()
            .unwrap_or_else(|_| src_dir.clone())
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| root.clone());
        let index = scan_source_files(&src_dir, &link_root)?;

        book.for_each_mut(|item| {
            if let BookItem::Chapter(chapter) = item {
                chapter.content = replace_refs(&chapter.content, &index, github_base.as_deref());
            }
        });

        append_coverage_chapters(ctx, &root, &index, github_base.as_deref(), &mut book)?;

        Ok(book)
    }

    fn supports_renderer(&self, _renderer: &str) -> anyhow::Result<bool> {
        Ok(true)
    }
}

/// Build a "Coverage report" chapter plus one subpage per judgment from the
/// scraped judgments and the recorded coverage JSONL, and append them to the
/// book. The JSONL is optional: when it is missing (e.g. CI builds the book
/// without running tests) `jsonl::read` yields empty coverage and every row
/// renders as uncovered rather than failing the build.
fn append_coverage_chapters(
    ctx: &PreprocessorContext,
    root: &Path,
    index: &SourceIndex,
    github_base: Option<&str>,
    book: &mut Book,
) -> anyhow::Result<()> {
    let jsonl_path: PathBuf = ctx
        .config
        .get("preprocessor.judgment.coverage-jsonl")
        .ok()
        .flatten()
        .map(|s: String| root.join(s))
        .unwrap_or_else(|| {
            root.parent()
                .unwrap_or(root)
                .join("target")
                .join("test-coverage.jsonl")
        });
    let cov = jsonl::read(&jsonl_path).unwrap_or_default();

    // Render in the same deterministic order `scrape_dir` uses, so the index
    // table is stable across builds.
    let mut judgments: Vec<Judgment> = index.judgments.values().cloned().collect();
    judgments.sort_by(|a, b| (a.file.as_str(), a.line).cmp(&(b.file.as_str(), b.line)));

    // mdbook derives sidebar nesting from each chapter's section-number depth,
    // not from `sub_items` (see the HTML renderer's TOC helper). So the index
    // gets the next top-level number and each subpage a two-component child
    // number, which renders them folded under the index.
    let next_top = book
        .iter()
        .filter_map(|item| match item {
            BookItem::Chapter(c) => c.number.as_ref().and_then(|n| n.first().copied()),
            _ => None,
        })
        .max()
        .unwrap_or(0)
        + 1;

    // Subpage filenames come from `report::slug` so they match the `./{slug}.md`
    // links `render_index` emits. Two judgments collapsing to one slug would
    // clobber each other's page; warn, mirroring `report::write_all`.
    let mut seen_slugs: HashSet<String> = HashSet::new();
    let mut subpages: Vec<BookItem> = Vec::new();
    for (i, j) in judgments.iter().enumerate() {
        let slug = report::slug(&j.name);
        if !seen_slugs.insert(slug.clone()) {
            eprintln!(
                "warning: slug collision for judgment `{}` at {}:{}, coverage subpage will overwrite a sibling",
                j.name, j.file, j.line,
            );
        }
        let mut chapter = Chapter::new(
            &j.name,
            report::render_subpage(j, &cov),
            PathBuf::from(format!("{slug}.md")),
            vec!["Coverage report".to_string()],
        );
        chapter.number = Some(SectionNumber::new(vec![next_top, (i + 1) as u32]));

        // Per-cell detail pages (the test lists each coverage cell links to)
        // hang off this judgment's subpage with a third section-number
        // component, so they fold under it in the sidebar.
        let mut detail_items: Vec<BookItem> = Vec::new();
        for (k, page) in report::render_detail_pages_for(j, &cov, github_base)
            .into_iter()
            .enumerate()
        {
            if !seen_slugs.insert(page.slug.clone()) {
                eprintln!(
                    "warning: slug collision for coverage detail page `{}`, it will overwrite a sibling",
                    page.slug,
                );
            }
            let mut detail = Chapter::new(
                &page.title,
                page.content,
                PathBuf::from(format!("{}.md", page.slug)),
                vec!["Coverage report".to_string(), j.name.clone()],
            );
            detail.number = Some(SectionNumber::new(vec![
                next_top,
                (i + 1) as u32,
                (k + 1) as u32,
            ]));
            detail_items.push(BookItem::Chapter(detail));
        }
        chapter.sub_items = detail_items;
        subpages.push(BookItem::Chapter(chapter));
    }

    let mut index_chapter = Chapter::new(
        "Coverage report",
        report::render_index(&judgments, &cov),
        PathBuf::from("coverage.md"),
        vec![],
    );
    index_chapter.number = Some(SectionNumber::new(vec![next_top]));
    index_chapter.sub_items = subpages;
    book.push_item(index_chapter);

    Ok(())
}

// --- Data structures ---

#[derive(Debug)]
pub struct SourceIndex {
    pub judgments: HashMap<String, Judgment>,
    pub anchors: HashMap<String, Anchor>,
}

#[derive(Debug)]
pub struct Anchor {
    pub name: String,
    pub content: String,
    pub file_path: String,
    pub line_number: usize,
}

// --- Source scanning ---

pub fn scan_source_files(src_dir: &Path, root: &Path) -> anyhow::Result<SourceIndex> {
    let mut judgments = HashMap::new();
    let mut anchors = HashMap::new();

    for entry in walk_rs_files(src_dir)? {
        let content = std::fs::read_to_string(&entry)?;
        let canonical_entry = entry.canonicalize().unwrap_or_else(|_| entry.clone());
        let rel_path = canonical_entry
            .strip_prefix(root)
            .unwrap_or(&canonical_entry)
            .to_string_lossy()
            .to_string();
        for judgment in parse_judgment_fns(&content, &rel_path) {
            judgments.insert(judgment.name.clone(), judgment);
        }
        for anchor in parse_anchors(&content, &rel_path) {
            anchors.insert(anchor.name.clone(), anchor);
        }
    }

    Ok(SourceIndex { judgments, anchors })
}

fn walk_rs_files(dir: &Path) -> anyhow::Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    if !dir.exists() {
        return Ok(files);
    }
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            files.extend(walk_rs_files(&path)?);
        } else if path.extension().is_some_and(|e| e == "rs") {
            files.push(path);
        }
    }
    Ok(files)
}

// --- Anchor parsing ---

pub fn parse_anchors(content: &str, file_path: &str) -> Vec<Anchor> {
    let mut anchors = Vec::new();
    let anchor_start_re = Regex::new(r"// ANCHOR: (\w+)").unwrap();

    for mat in anchor_start_re.find_iter(content) {
        let caps = anchor_start_re.captures(&content[mat.start()..]).unwrap();
        let name = caps.get(1).unwrap().as_str().to_string();
        let line_number = content[..mat.start()].matches('\n').count() + 1;

        let end_marker = format!("// ANCHOR_END: {name}");
        let after_start = mat.end();
        if let Some(end_offset) = content[after_start..].find(&end_marker) {
            let anchor_content = content[after_start..after_start + end_offset]
                .trim_start_matches('\n')
                .trim_end_matches('\n')
                .to_string();

            anchors.push(Anchor {
                name,
                content: dedent(&anchor_content),
                file_path: file_path.to_string(),
                line_number,
            });
        }
    }

    anchors
}

fn dedent(text: &str) -> String {
    let lines: Vec<&str> = text.lines().collect();
    let min_indent = lines
        .iter()
        .filter(|l| !l.trim().is_empty())
        .map(|l| l.len() - l.trim_start().len())
        .min()
        .unwrap_or(0);

    lines
        .iter()
        .map(|l| {
            if l.len() >= min_indent {
                &l[min_indent..]
            } else {
                l.trim()
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

// --- Rendering ---

fn render_figure(
    css_class: &str,
    id: &str,
    label: &str,
    src_link: Option<&str>,
    code: &str,
    doc: Option<&str>,
) -> String {
    let src_tag = match src_link {
        Some(link) => format!(
            "\n<a class=\"judgment-src\" href=\"{link}\" title=\"View source\" target=\"_blank\">[src]</a>"
        ),
        None => String::new(),
    };

    let mut out = format!(
        "<figure class=\"{css_class}\" id=\"{id}\">\n\
         <figcaption>\n\
         <a href=\"#{id}\">{label}</a>{src_tag}\n\
         </figcaption>\n\
         \n\
         ```rust,ignore\n\
         {code}\n\
         ```\n",
    );

    if let Some(doc) = doc {
        out.push('\n');
        out.push_str(doc);
        out.push('\n');
    }

    out.push_str("\n</figure>\n");
    out
}

fn source_link(github_base: Option<&str>, file_path: &str, line: u32) -> Option<String> {
    github_base.map(|base| format!("{base}/{file_path}#L{line}"))
}

fn render_rule(judgment: &Judgment, rule_name: &str, github_base: Option<&str>) -> String {
    match judgment.rules.iter().find(|r: &&Rule| r.name == rule_name) {
        Some(rule) => {
            let link = source_link(github_base, &judgment.file, rule.line);
            let id = format!("judgment-{}--{}", judgment.name, rule_name);
            let label = format!("{}::{}", judgment.name, rule_name);
            render_figure(
                "judgment-rule",
                &id,
                &label,
                link.as_deref(),
                &rule.raw_text,
                None,
            )
        }
        None => {
            eprintln!(
                "warning: rule `{}` not found in judgment `{}`",
                rule_name, judgment.name
            );
            format!(
                "**[rule `{}` not found in `{}`]**",
                rule_name, judgment.name
            )
        }
    }
}

fn render_judgment(judgment: &Judgment, github_base: Option<&str>) -> String {
    let link = source_link(github_base, &judgment.file, judgment.line);
    let id = format!("judgment-{}", judgment.name);
    let doc = if judgment.doc_comment.is_empty() {
        None
    } else {
        Some(judgment.doc_comment.as_str())
    };
    render_figure(
        "judgment",
        &id,
        &judgment.name,
        link.as_deref(),
        &judgment.signature,
        doc,
    )
}

fn render_anchor(anchor: &Anchor, github_base: Option<&str>) -> String {
    let link = source_link(github_base, &anchor.file_path, anchor.line_number as u32);
    let id = format!("anchor-{}", anchor.name);
    render_figure(
        "anchor",
        &id,
        &anchor.name,
        link.as_deref(),
        &anchor.content,
        None,
    )
}

// --- Markdown replacement ---

pub fn replace_refs(content: &str, index: &SourceIndex, github_base: Option<&str>) -> String {
    let rule_re = Regex::new(r#"\{judgment-rule\}`(\w+),\s*([^`]+)`"#).unwrap();
    let content = rule_re.replace_all(content, |caps: &regex::Captures| {
        let fn_name = &caps[1];
        let rule_name = caps[2].trim();

        match index.judgments.get(fn_name) {
            Some(judgment) => render_rule(judgment, rule_name, github_base),
            None => {
                eprintln!("warning: judgment function `{fn_name}` not found");
                format!("**[judgment `{fn_name}` not found]**")
            }
        }
    });

    let judgment_re = Regex::new(r#"\{judgment\}`(\w+)`"#).unwrap();
    let content = judgment_re
        .replace_all(&content, |caps: &regex::Captures| {
            let fn_name = &caps[1];

            match index.judgments.get(fn_name) {
                Some(judgment) => render_judgment(judgment, github_base),
                None => {
                    eprintln!("warning: judgment function `{fn_name}` not found");
                    format!("**[judgment `{fn_name}` not found]**")
                }
            }
        })
        .to_string();

    let anchor_re = Regex::new(r#"\{anchor\}`(\w+)`"#).unwrap();
    let content = anchor_re
        .replace_all(&content, |caps: &regex::Captures| {
            let anchor_name = &caps[1];

            match index.anchors.get(anchor_name) {
                Some(anchor) => render_anchor(anchor, github_base),
                None => {
                    eprintln!("warning: anchor `{anchor_name}` not found");
                    format!("**[anchor `{anchor_name}` not found]**")
                }
            }
        })
        .to_string();

    replace_mermaid_blocks(&content)
}

fn replace_mermaid_blocks(content: &str) -> String {
    let mermaid_re = Regex::new(r"(?s)```mermaid\s*\n(.*?)```").unwrap();
    if !mermaid_re.is_match(content) {
        return content.to_string();
    }

    let content = mermaid_re
        .replace_all(content, |caps: &regex::Captures| {
            let diagram = caps[1].trim();
            format!("<pre class=\"mermaid\">\n{diagram}\n</pre>")
        })
        .to_string();

    const MERMAID_SCRIPT: &str = concat!(
        "<script type=\"module\">\n",
        "import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs';\n",
        "mermaid.initialize({ startOnLoad: true });\n",
        "</script>\n",
    );

    format!("{content}\n{MERMAID_SCRIPT}")
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r#"
judgment_fn! {
    /// Compute the move/copy behavior for a place access.
    fn move_place(
        env: Env,
        live_after: LivePlaces,
        place: Place,
        ty: Ty,
    ) => Env {
        debug(place, ty, env, live_after)

        (
            (if live_after.is_live(&place))!
            (prove_is_copy(&env, ty) => ())
            ----------------------------------- ("copy")
            (move_place(env, _live_after, _place, ty) => &env)
        )

        (
            (if !live_after.is_live(&place))
            (let env = env.with_place_in_flight(&place))
            ----------------------------------- ("give")
            (move_place(env, live_after, place, _ty) => env)
        )
    }
}
"#;

    const ANCHOR_SAMPLE: &str = r#"
some preamble
// ANCHOR: Env
pub struct Env {
    program: Arc<Program>,
    local_variables: Map<Var, Ty>,
}
// ANCHOR_END: Env
some postamble
"#;

    const GITHUB_BASE: &str = "https://github.com/example/repo/blob/main";

    fn make_index() -> SourceIndex {
        let judgments = parse_judgment_fns(SAMPLE, "src/type_system/expressions.rs");
        let mut judgment_map = HashMap::new();
        for j in judgments {
            judgment_map.insert(j.name.clone(), j);
        }

        let anchors = parse_anchors(ANCHOR_SAMPLE, "src/type_system/env.rs");
        let mut anchor_map = HashMap::new();
        for a in anchors {
            anchor_map.insert(a.name.clone(), a);
        }

        SourceIndex {
            judgments: judgment_map,
            anchors: anchor_map,
        }
    }

    #[test]
    fn test_judgment_shows_signature_only() {
        let index = make_index();
        let input = "The judgment: {judgment}`move_place`";
        let output = replace_refs(input, &index, Some(GITHUB_BASE));
        assert!(output.contains("move_place("), "output: {output}");
        assert!(output.contains("judgment-src"), "output: {output}");
        assert!(!output.contains("prove_is_copy"), "output: {output}");
    }

    #[test]
    fn test_judgment_rule_shows_rule_with_link() {
        let index = make_index();
        let input = r#"The copy rule: {judgment-rule}`move_place, copy`"#;
        let output = replace_refs(input, &index, Some(GITHUB_BASE));
        assert!(output.contains("prove_is_copy"), "output: {output}");
        assert!(output.contains("github.com"), "output: {output}");
        assert!(output.contains("move_place::copy"), "output: {output}");
    }

    #[test]
    fn test_both_in_same_content() {
        let index = make_index();
        let input = r#"Sig: {judgment}`move_place`

Rule: {judgment-rule}`move_place, copy`"#;
        let output = replace_refs(input, &index, Some(GITHUB_BASE));
        assert!(output.contains("move_place("), "output: {output}");
        assert!(output.contains("prove_is_copy"), "output: {output}");
        assert!(!output.contains("{judgment"), "output: {output}");
    }

    #[test]
    fn test_parse_anchors() {
        let anchors = parse_anchors(ANCHOR_SAMPLE, "src/type_system/env.rs");
        assert_eq!(anchors.len(), 1);
        let a = &anchors[0];
        assert_eq!(a.name, "Env");
        assert!(
            a.content.contains("pub struct Env"),
            "content: {}",
            a.content
        );
        assert!(
            a.content.contains("local_variables"),
            "content: {}",
            a.content
        );
        assert_eq!(a.file_path, "src/type_system/env.rs");
        assert_eq!(a.line_number, 3);
    }

    #[test]
    fn test_anchor_replacement() {
        let index = make_index();
        let input = "The env: {anchor}`Env`";
        let output = replace_refs(input, &index, Some(GITHUB_BASE));
        assert!(output.contains("pub struct Env"), "output: {output}");
        assert!(output.contains("github.com"), "output: {output}");
        assert!(output.contains("anchor-Env"), "output: {output}");
        assert!(output.contains("[src]"), "output: {output}");
        assert!(!output.contains("{anchor}"), "output: {output}");
    }

    #[test]
    fn test_no_github_base_omits_src_link() {
        let index = make_index();
        let input = "The env: {anchor}`Env`";
        let output = replace_refs(input, &index, None);
        assert!(output.contains("pub struct Env"), "output: {output}");
        assert!(!output.contains("[src]"), "output: {output}");
        assert!(!output.contains("judgment-src"), "output: {output}");
    }
}
