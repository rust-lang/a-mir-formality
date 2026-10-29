use formality_mdbook_skills::SkillsPreprocessor;
use mdbook_preprocessor::book::{Book, BookItem, Chapter};
use mdbook_preprocessor::config::Config;
use mdbook_preprocessor::{Preprocessor, PreprocessorContext};

#[test]
fn expands_nested_chapters_relative_to_source_path() {
    let root = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(root.path().join("src/nested")).unwrap();
    std::fs::create_dir(root.path().join("my skills")).unwrap();
    std::fs::write(
        root.path().join("my skills/SKILL.md"),
        "---\nname: example\n---\n# Included skill\n",
    )
    .unwrap();
    let mut parent = Chapter::new("Parent", String::new(), "parent.md", vec![]);
    let mut child = Chapter::new(
        "Child",
        "Before\n{{#skill ../../my skills/SKILL.md}}\nMiddle\n{{#skill ../../my skills/SKILL.md}}\nAfter".into(),
        "nested/chapter.md",
        vec![],
    );
    // Rendered paths may differ from the actual source paths (e.g. README.md).
    child.path = Some("different/output.md".into());
    parent.sub_items.push(BookItem::Chapter(child));
    let ctx = PreprocessorContext::new(root.path().into(), Config::default(), "html".into());
    let book = SkillsPreprocessor
        .run(&ctx, Book::new_with_items(vec![BookItem::Chapter(parent)]))
        .unwrap();
    let child = book.chapters().nth(1).unwrap();
    assert_eq!(
        child.content,
        "Before\n# Included skill\n\nMiddle\n# Included skill\n\nAfter"
    );
}

#[test]
fn missing_skill_reports_chapter_and_skill_paths() {
    let root = tempfile::tempdir().unwrap();
    let ctx = PreprocessorContext::new(root.path().into(), Config::default(), "html".into());
    let chapter = Chapter::new(
        "Missing",
        "{{#skill absent/SKILL.md}}".into(),
        "chapter.md",
        vec![],
    );
    let error = SkillsPreprocessor
        .run(&ctx, Book::new_with_items(vec![BookItem::Chapter(chapter)]))
        .unwrap_err();
    let message = format!("{error:#}");
    assert!(message.contains("chapter.md"), "{message}");
    assert!(message.contains("absent/SKILL.md"), "{message}");
}

#[test]
fn binary_supports_renderers_and_rejects_invalid_arguments() {
    let binary = env!("CARGO_BIN_EXE_formality-mdbook-skills");
    assert!(
        std::process::Command::new(binary)
            .args(["supports", "html"])
            .status()
            .unwrap()
            .success()
    );
    let output = std::process::Command::new(binary)
        .arg("supports")
        .output()
        .unwrap();
    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("usage:"));
}
