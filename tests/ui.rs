use ui_test::clap::Parser;
use ui_test::color_eyre::Result;
use ui_test::*;

fn main() -> Result<()> {
    let mut config = Config::rustc("tests/ui");
    // Prevent ui_test from trying to query `formality` for the host platform name
    config.host = Some("non of ya bizniz".into());
    config.program.program = "target/debug/formality".into();
    config.mode = Mode::Fail {
        require_patterns: false,
    };

    let args = Args::parse();
    let text = if args.quiet {
        status_emitter::Text::quiet()
    } else {
        status_emitter::Text::verbose()
    };
    if !args.check && std::env::var_os("GITHUB_ACTIONS").is_none() {
        config.output_conflict_handling = OutputConflictHandling::Bless;
    }

    run_tests_generic(
        config,
        args,
        |path, args| {
            path.extension().is_some_and(|ext| ext == "🔬") && default_filter_by_arg(path, args)
        },
        default_per_file_config,
        (text, status_emitter::Gha::<true> { name: "ui".into() }),
    )
}
