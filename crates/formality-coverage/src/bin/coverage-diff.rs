//! Compare a PR's coverage summary against a published baseline and print a
//! markdown report to stdout (feed it into `$GITHUB_STEP_SUMMARY`).
//!
//! The baseline is the `coverage-summary.json` published alongside the book on
//! the last `main` build; the head is the summary this PR's build produced. A
//! missing baseline is treated as informational, never an error, so the job
//! stays green until a baseline has been published at least once.

use anyhow::Result;
use clap::Parser;
use std::path::PathBuf;

use formality_coverage::{diff, summary};

#[derive(Parser, Debug)]
#[command(about = "Diff a coverage summary against a baseline and print markdown")]
struct Args {
    /// Baseline summary (e.g. fetched from the published book).
    #[arg(long)]
    base: PathBuf,

    /// This build's summary, as written by `formality-coverage --summary-json`.
    #[arg(long)]
    head: PathBuf,

    /// Exit non-zero when a rule lost coverage or a new rule is untested.
    /// Off by default: the report is informational until the team opts in.
    #[arg(long)]
    fail_on_regression: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();

    if !args.base.exists() {
        println!(
            "_No coverage baseline found at `{}`; skipping comparison. \
             A baseline is published on the next `main` build._",
            args.base.display(),
        );
        return Ok(());
    }

    let base = summary::read(&args.base)?;
    let head = summary::read(&args.head)?;

    if base.schema_version != head.schema_version {
        println!(
            "_Coverage summary schema changed (baseline v{}, this build v{}); \
             skipping comparison until the baseline is republished._",
            base.schema_version, head.schema_version,
        );
        return Ok(());
    }

    let report = diff::diff(&base, &head);
    print!("{}", report.to_markdown());

    if args.fail_on_regression && !report.is_clean() {
        std::process::exit(1);
    }
    Ok(())
}
