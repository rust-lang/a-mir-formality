use rustc_formality::{self, OutputFormat};
use std::{env, path::Path, process::ExitCode};

fn main() -> ExitCode {
    // the path to the Rust source file is specified as an argument to the program
    let args = env::args().skip(1).collect::<Vec<_>>();
    let expect_failure = args.iter().any(|s| s == "--fail");
    let input_path = args
        .iter()
        .find(|arg| !arg.starts_with("--"))
        .expect("Specify input path");

    let generated_code =
        rustc_formality::run_rustc(input_path, OutputFormat::Expression, expect_failure)
            .expect("Failed to run rustc");

    let output_path = Path::new(input_path).with_extension("rkt");
    std::fs::write(output_path, &generated_code).unwrap();

    if rustc_formality::run_racket(&generated_code).expect("Failed to run racket") {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
