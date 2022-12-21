use formality_types::grammar::Fallible;

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

fn main() -> Fallible<()> {
    let args = Args::parse();
    let input = std::fs::read_to_string(&args.input_path)?;
    formality_rust::test_program_ok(&input)?;
    Ok(())
}
