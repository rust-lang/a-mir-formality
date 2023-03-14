use clap::Parser;
use formality_check::check_all_crates;
use formality_rust::grammar::Program;
use formality_types::parse::term;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(long)]
    print_rust: bool,

    input_path: String,
}

pub fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let input: String = std::fs::read_to_string(&args.input_path)?;
    let program: Program = term(&input);

    if args.print_rust {
        eprintln!("{:#?}", program);
    }

    check_all_crates(&program)
}

pub fn test_program_ok(input: &str) -> anyhow::Result<()> {
    let program: Program = term(&input);
    check_all_crates(&program)
}
