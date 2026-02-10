use std::{path::PathBuf, sync::Arc};

use clap::Parser;
use formality_core::judgment::ProofTree;
use formality_rust::check::check_all_crates;
use formality_rust::grammar::Program;
use formality_rust::prove::prove::{test_util::TestAssertion, Constraints};
use formality_types::rust::try_term;

#[cfg(test)]
mod test;

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
    let program: Program = try_term(&input)?;

    if args.print_rust {
        eprintln!("{:#?}", program);
    }

    let _proof_tree = check_all_crates(&program)?;
    Ok(())
}

#[macro_export]
macro_rules! assert_ok {
    ($input:tt) => {{
        let _ = $crate::test_program_ok(stringify!($input)).expect("expected program to pass");
    }};
}

#[macro_export]
macro_rules! assert_err {
    ($input:tt [$($must_have:expr,)*] $expect:expr) => {{
        use formality_core::test_util::AnyhowResultTestExt;
        $crate::test_program_ok(stringify!($input)).assert_has_err_leaves($expect, &[$($must_have,)*]);
    }};
}

pub fn test_program_ok(input: &str) -> anyhow::Result<ProofTree> {
    let program: Program = try_term(input)?;
    let proof_tree = check_all_crates(&program)?;
    Ok(proof_tree)
}

pub fn test_where_clause(program: &str, assertion: &str) -> formality_core::ProvenSet<Constraints> {
    formality_core::with_tracing_logs(|| {
        let program: Program = try_term(program).unwrap();
        let _proof_tree = check_all_crates(&program).unwrap();
        let assertion: Arc<TestAssertion> = try_term(assertion).unwrap();
        let decls = program.to_prove_decls();
        formality_rust::prove::prove::test_util::test_prove(decls, assertion)
    })
}
