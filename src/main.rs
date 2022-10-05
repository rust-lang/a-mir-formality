use rustc_formality::{self, OutputFormat};
use std::process::ExitCode;

use clap::Parser;

/// Formality test generator: convert Rust programs into redex tests
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// If set, print the racket program to stdout and quit.
    /// Otherwise, we will invoke racket.
    #[arg(long)]
    print: bool,

    /// If set, generate a test that expects the program not to compile.
    #[arg(long)]
    fail: bool,

    input_path: String,
}

fn main() -> ExitCode {
    let args = Args::parse();
    // the path to the Rust source file is specified as an argument to the program

    let generated_code =
        rustc_formality::run_rustc(&args.input_path, OutputFormat::Expression, args.fail)
            .expect("Failed to run rustc");

    if args.print {
        println!("{generated_code}");
        ExitCode::SUCCESS
    } else {
        if rustc_formality::run_racket(&generated_code).expect("Failed to run racket") {
            ExitCode::SUCCESS
        } else {
            ExitCode::FAILURE
        }
    }
}
