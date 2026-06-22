use std::{path::PathBuf, sync::Arc};

use clap::Parser;
use formality_core::judgment::ProofTree;
use formality_rust::check::check_all_crates;
use formality_rust::grammar::Crates;
use formality_rust::prove::prove::test_util::TestAssertion;
use formality_rust::prove::prove::Env;
use formality_rust::rust::try_term;

pub mod test_util;
pub use test_util::FormalityTest;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(long)]
    print_rust: bool,

    #[arg(long, default_value = "")]
    error_format: String,

    #[arg(long, default_value = "")]
    crate_type: String,

    #[arg(long, default_value = "")]
    edition: String,

    #[arg(long)]
    out_dir: Option<PathBuf>,

    input_path: String,
}

pub fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let input: String = std::fs::read_to_string(&args.input_path)?;
    let program: Crates = try_term(&input)?;

    if args.print_rust {
        eprintln!("{:#?}", program);
    }

    let _proof_tree = check_all_crates(&program).check_proven()?;
    Ok(())
}

pub fn test_program_ok(input: &str) -> anyhow::Result<ProofTree> {
    let program: Crates = try_term(input)?;
    let proof_tree = check_all_crates(&program).check_proven()?;
    Ok(proof_tree)
}

pub fn test_where_clause(program: &str, assertion: &str) -> formality_core::ProvenSet<Env> {
    formality_core::with_tracing_logs(|| {
        let program: Crates = try_term(program).unwrap();
        let _proof_tree = check_all_crates(&program).check_proven().unwrap();
        let assertion: Arc<TestAssertion> = try_term(assertion).unwrap();
        let decls = program.to_prove_decls();
        formality_rust::prove::prove::test_util::test_prove(decls, assertion)
    })
}

pub fn run_rustc() -> bool {
    0 < std::env::var("FORMALITY_RUN_RUSTC")
        .map(|s| s.parse::<i32>())
        .unwrap_or(Ok(0))
        .unwrap_or_default()
}
