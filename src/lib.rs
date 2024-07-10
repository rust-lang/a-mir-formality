#![feature(rustc_private)]
#![feature(control_flow_enum)]

#[macro_use]
extern crate rustc_smir;
extern crate rustc_driver;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate stable_mir;

use std::{path::PathBuf, sync::Arc};

use clap::Parser;
use formality_check::check_all_crates;
use formality_core::Set;
use formality_prove::{test_util::TestAssertion, Constraints};
use formality_rust::{fuzz::FuzzProgram, grammar::Program};
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

    #[arg(long, default_value = "0")]
    generate_fuzzed_programs: usize,

    input_paths: Vec<String>,
}

pub fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    // Check any of the input paths for correctness.
    for input_path in &args.input_paths {
        match check_input(&args, input_path) {
            Ok(()) => {}
            Err(err) => {
                eprintln!("path {} failed to compile:\n{:#?}", input_path, err)
            }
        }
    }

    generate_fuzzed_program(&args);

    Ok(())
}

fn check_input(args: &Args, input_path: &str) -> anyhow::Result<()> {
    let input: String = std::fs::read_to_string(input_path)?;
    let program: Program = try_term(&input)?;

    if args.print_rust {
        eprintln!("{:#?}", program);
    }

    check_all_crates(&program)
}

fn generate_fuzzed_program(args: &Args) {
    use bolero::check;

    check!()
        .with_generator(FuzzProgram::default().num_generic_parameters(0..1))
        .with_iterations(args.generate_fuzzed_programs)
        .for_each(|p| eprintln!("{p:?}"));
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
