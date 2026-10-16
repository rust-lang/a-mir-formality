//! Include skill Markdown in mdBook chapters without exposing YAML metadata.

use std::path::Path;

use anyhow::{Context, Result, bail};
use mdbook_preprocessor::book::{Book, BookItem};
use mdbook_preprocessor::{Preprocessor, PreprocessorContext};
use regex::Regex;

/// Expands `{{#skill path/to/SKILL.md}}` relative to each chapter's source file.
///
/// Skills are inserted once, verbatim except for their leading YAML frontmatter.
/// Nested skill directives are not expanded. Run before mdBook's `links`
/// preprocessor if included bodies contain ordinary mdBook include directives.
#[derive(Debug, Default)]
pub struct SkillsPreprocessor;

impl Preprocessor for SkillsPreprocessor {
    fn name(&self) -> &str {
        "skills"
    }

    fn run(&self, ctx: &PreprocessorContext, mut book: Book) -> Result<Book> {
        let directive = Regex::new(r"\{\{#skill\s+([^{}\r\n]+)\}\}")?;
        let source_root = ctx.root.join(&ctx.config.book.src);
        expand_items(&mut book.items, &source_root, &directive)?;
        Ok(book)
    }
}

fn expand_items(items: &mut [BookItem], source_root: &Path, directive: &Regex) -> Result<()> {
    for item in items {
        if let BookItem::Chapter(chapter) = item {
            if let Some(source_path) = &chapter.source_path {
                let path = source_root.join(source_path);
                let directory = path.parent().context("chapter has no parent directory")?;
                chapter.content = expand(&chapter.content, directory, directive)
                    .with_context(|| format!("failed to include skill in {}", path.display()))?;
            }
            expand_items(&mut chapter.sub_items, source_root, directive)?;
        }
    }
    Ok(())
}

fn expand(content: &str, directory: &Path, directive: &Regex) -> Result<String> {
    let mut output = String::new();
    let mut end = 0;
    for captures in directive.captures_iter(content) {
        let matched = captures.get(0).expect("regex always has a full match");
        output.push_str(&content[end..matched.start()]);
        let path = directory.join(captures[1].trim());
        let skill = std::fs::read_to_string(&path)
            .with_context(|| format!("failed to read skill {}", path.display()))?;
        let body =
            skill_body(&skill).with_context(|| format!("invalid skill {}", path.display()))?;
        output.push_str(body);
        end = matched.end();
    }
    output.push_str(&content[end..]);
    Ok(output)
}

fn skill_body(skill: &str) -> Result<&str> {
    let skill = skill.strip_prefix('\u{feff}').unwrap_or(skill);
    let mut lines = skill.split_inclusive('\n');
    let Some(first) = lines.next() else {
        return Ok(skill);
    };
    if first.trim_end() != "---" {
        return Ok(skill);
    }
    let mut offset = first.len();
    for line in lines {
        if matches!(line.trim_end(), "---" | "...") {
            let metadata: serde_yaml_ng::Value =
                serde_yaml_ng::from_str(&skill[first.len()..offset])
                    .context("failed to parse YAML frontmatter")?;
            if !metadata.is_mapping() {
                bail!("YAML frontmatter must be a mapping");
            }
            return Ok(&skill[offset + line.len()..]);
        }
        offset += line.len();
    }
    bail!("YAML frontmatter is missing its closing delimiter")
}

#[cfg(test)]
mod tests {
    use super::skill_body;

    #[test]
    fn preserves_markdown_and_removes_metadata() {
        let body = "\n# Skill\n\n```rust\nfn example() {}\n```\n\n---\n";
        let skill = format!("---\nname: example\ndescription: |\n  A skill\n---\n{body}");
        assert_eq!(skill_body(&skill).unwrap(), body);
        assert_eq!(skill_body(body).unwrap(), body);
    }

    #[test]
    fn accepts_crlf_bom_and_yaml_end_marker() {
        assert_eq!(
            skill_body("\u{feff}---\r\nname: example\r\n...\r\n# Skill\r\n").unwrap(),
            "# Skill\r\n"
        );
    }

    #[test]
    fn rejects_malformed_frontmatter() {
        for skill in [
            "---\nname: missing end",
            "---\nname: [\n---\nbody",
            "---\n- list\n---\nbody",
        ] {
            assert!(skill_body(skill).is_err(), "accepted {skill:?}");
        }
    }
}
