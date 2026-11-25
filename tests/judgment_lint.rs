use anyhow::{Context, Result};
use std::fs;
use std::path::Path;

#[test]
fn judgment_functions_pass_lints() -> Result<()> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut directories = vec![root.join("crates")];
    let mut files = Vec::new();
    while let Some(directory) = directories.pop() {
        for entry in
            fs::read_dir(&directory).with_context(|| format!("reading {}", directory.display()))?
        {
            let entry = entry?;
            let kind = entry.file_type()?;
            let path = entry.path();
            if kind.is_dir() {
                directories.push(path);
            } else if kind.is_file() && path.extension().is_some_and(|ext| ext == "rs") {
                files.push(path);
            }
        }
    }
    files.sort();
    assert!(!files.is_empty(), "no Rust sources found to lint");

    let mut diagnostics = Vec::new();
    for path in files {
        let source =
            fs::read_to_string(&path).with_context(|| format!("reading {}", path.display()))?;
        for finding in formality_lint::lint(&source) {
            diagnostics.push(finding.render(&path.strip_prefix(root)?.to_string_lossy(), &source));
        }
    }
    assert!(diagnostics.is_empty(), "{}", diagnostics.join("\n\n"));
    Ok(())
}
