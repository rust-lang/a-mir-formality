use std::io;
use std::process;

use formality_mdbook::JudgmentPreprocessor;
use mdbook_preprocessor::Preprocessor;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() > 1 && args[1] == "supports" {
        process::exit(0);
    }

    let (ctx, book) = mdbook_preprocessor::parse_input(io::stdin()).expect("failed to parse input");
    let preprocessor = JudgmentPreprocessor;
    let processed = preprocessor
        .run(&ctx, book)
        .expect("failed to run preprocessor");
    serde_json::to_writer(io::stdout(), &processed).expect("failed to write output");
}
