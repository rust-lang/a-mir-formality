// use clap::Parser;
// use formality_rust::grammar::Program;
// use formality_types::parse::term;

// /// Formality test generator: convert Rust programs into redex tests
// #[derive(Parser, Debug)]
// #[command(author, version, about, long_about = None)]
// struct Args {
//     #[arg(long)]
//     print_rust: bool,

//     #[arg(long)]
//     print_decl: bool,

//     input_path: String,
// }

// fn main() -> anyhow::Result<()> {
//     let args = Args::parse();
//     let input: String = std::fs::read_to_string(&args.input_path)?;
//     let program: Program = term(&input);

//     if args.print_rust {
//         eprintln!("{:#?}", program);
//     }

//     let decl = program.to_decl()?;

//     if args.print_decl {
//         eprintln!("{:#?}", decl);
//     }

//     Ok(())
// }

fn main() {
    println!("FIXME");
}
