//! CLI that scrapes the source tree, reads the positive-coverage JSONL,
//! and writes a markdown report.

use anyhow::Result;
use clap::Parser;
use std::path::PathBuf;

use formality_coverage::{jsonl, report, scrape, summary};

#[derive(Parser, Debug)]
#[command(about = "Generate a markdown coverage report for a-mir-formality judgments")]
struct Args {
    /// Directory to scan for `judgment_fn!` invocations.
    #[arg(long, default_value = "crates")]
    src_root: PathBuf,

    /// Path to the JSONL coverage file produced by positive tests.
    #[arg(long, default_value = "target/test-coverage.jsonl")]
    jsonl: PathBuf,

    /// Directory where the markdown report will be written.
    #[arg(long, default_value = "target/coverage-report")]
    out_dir: PathBuf,

    /// Base URL for test links, e.g.
    /// `https://github.com/rust-lang/a-mir-formality/blob/main`. When omitted,
    /// ✓ marks render as plain text instead of links.
    #[arg(long)]
    github_base: Option<String>,

    /// When set, also write a compact machine-readable coverage summary here.
    /// This is the small JSON the published book carries as a CI baseline.
    #[arg(long)]
    summary_json: Option<PathBuf>,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let judgments = scrape::scrape_dir(&args.src_root)?;

    if !args.jsonl.exists() {
        eprintln!(
            "note: no coverage data at {}; did you run `cargo test` first? \
             Continuing with empty coverage.",
            args.jsonl.display(),
        );
    }
    let cov = jsonl::read(&args.jsonl)?;

    let positive_count = cov.positive.len();
    let negative_premise_count = cov.negative_premises.len();
    let github_base = args.github_base.as_deref().map(|s| s.trim_end_matches('/'));
    // Test source locations recorded in the JSONL are relative to the repo
    // root, which is the directory the CLI is run from.
    report::write_all(
        &args.out_dir,
        &judgments,
        &cov,
        github_base,
        Some(std::path::Path::new(".")),
    )?;
    eprintln!(
        "wrote {} judgments ({} positive rules, {} negatively-tested premises) to {}",
        judgments.len(),
        positive_count,
        negative_premise_count,
        args.out_dir.display(),
    );

    if let Some(path) = &args.summary_json {
        let summary = summary::build(&judgments, &cov);
        summary::write(path, &summary)?;
        eprintln!("wrote coverage summary to {}", path.display());
    }

    Ok(())
}
