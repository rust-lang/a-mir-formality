use std::{path::PathBuf, sync::Arc};

use clap::Parser;
use formality_check::check_all_crates;
use formality_core::Set;
use formality_prove::{test_util::TestAssertion, Constraints};
use formality_rust::grammar::Program;
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

    check_all_crates(&program)
}

#[macro_export]
macro_rules! assert_ok {
    ($input:tt $expect:expr) => {{
        use formality_core::test_util::ResultTestExt;
        $crate::test_program_ok(stringify!($input)).assert_ok($expect);
    }};
}

#[macro_export]
macro_rules! assert_err {
    ($input:tt [$($must_have:expr,)*] $expect:expr) => {{
        use formality_core::test_util::ResultTestExt;
        $crate::test_program_ok(stringify!($input)).assert_has_err($expect, &[$($must_have,)*]);
    }};
}

pub fn test_program_ok(input: &str) -> anyhow::Result<()> {
    let program: Program = try_term(input)?;
    check_all_crates(&program)
}

pub fn test_where_clause(program: &str, assertion: &str) -> anyhow::Result<Set<Constraints>> {
    formality_core::with_tracing_logs(|| {
        let program: Program = try_term(program)?;
        check_all_crates(&program)?;
        let assertion: Arc<TestAssertion> = try_term(assertion)?;
        let decls = program.to_prove_decls();
        Ok(formality_prove::test_util::test_prove(decls, assertion).into_set()?)
    })
}
