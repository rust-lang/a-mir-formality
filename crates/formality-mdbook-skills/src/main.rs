use std::io;

use anyhow::{Context, Result, bail};
use formality_mdbook_skills::SkillsPreprocessor;
use mdbook_preprocessor::Preprocessor;

fn main() -> Result<()> {
    let args: Vec<_> = std::env::args().skip(1).collect();
    match args.as_slice() {
        [command, _renderer] if command == "supports" => return Ok(()),
        [] => {}
        _ => bail!("usage: formality-mdbook-skills [supports <renderer>]"),
    }
    let (ctx, book) = mdbook_preprocessor::parse_input(io::stdin())?;
    let processed = SkillsPreprocessor.run(&ctx, book)?;
    serde_json::to_writer(io::stdout(), &processed).context("failed to write processed book")
}
