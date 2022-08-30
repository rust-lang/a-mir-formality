#![feature(rustc_private)]
#![feature(iter_intersperse)]

// rustup component add rustc-dev llvm-tools-preview
// version: rustc 1.65.0-nightly (29e4a9ee0 2022-08-10)

extern crate rustc_error_codes;
extern crate rustc_errors;
extern crate rustc_hash;
extern crate rustc_hir;
extern crate rustc_infer;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;

use std::{
    io::{self, Write},
    path,
    process::{self, Command},
    str,
};

use rustc_errors::registry;
use rustc_hash::{FxHashMap, FxHashSet};
use rustc_session::config::{self, CheckCfg};

mod gen;
mod renumber_mir;

pub enum OutputFormat {
    Expression,
    TestModule,
}

pub fn run_rustc(
    input_path: &str,
    output_format: OutputFormat,
    expect_failure: bool,
) -> io::Result<String> {
    let out = process::Command::new("rustc")
        .arg("--print=sysroot")
        .current_dir(".")
        .output()?;

    let sysroot = str::from_utf8(&out.stdout).unwrap().trim();

    let config = rustc_interface::Config {
        // Command line options
        opts: config::Options {
            maybe_sysroot: Some(path::PathBuf::from(sysroot)),
            ..config::Options::default()
        },
        // cfg! configuration in addition to the default ones
        crate_cfg: FxHashSet::default(), // FxHashSet<(String, Option<String>)>
        crate_check_cfg: CheckCfg::default(), // CheckCfg
        input: config::Input::File(path::PathBuf::from(input_path)),
        input_path: Some(path::PathBuf::from(input_path)), // Option<PathBuf>
        output_dir: None,                                  // Option<PathBuf>
        output_file: None,                                 // Option<PathBuf>
        file_loader: None, // Option<Box<dyn FileLoader + Send + Sync>>
        diagnostic_output: rustc_session::DiagnosticOutput::Default,
        lint_caps: FxHashMap::default(), // FxHashMap<lint::LintId, lint::Level>
        // This is a callback from the driver that is called when [`ParseSess`] is created.
        parse_sess_created: None, //Option<Box<dyn FnOnce(&mut ParseSess) + Send>>
        // This is a callback from the driver that is called when we're registering lints;
        // it is called during plugin registration when we have the LintStore in a non-shared state.
        //
        // Note that if you find a Some here you probably want to call that function in the new
        // function being registered.
        register_lints: None, // Option<Box<dyn Fn(&Session, &mut LintStore) + Send + Sync>>
        // This is a callback from the driver that is called just after we have populated
        // the list of queries.
        //
        // The second parameter is local providers and the third parameter is external providers.
        override_queries: None, // Option<fn(&Session, &mut ty::query::Providers<'_>, &mut ty::query::Providers<'_>)>
        // Registry of diagnostics codes.
        registry: registry::Registry::new(&rustc_error_codes::DIAGNOSTICS),
        make_codegen_backend: None,
    };

    Ok(rustc_interface::run_compiler(config, |compiler| {
        compiler.enter(|queries| {
            // Analyze the program and inspect the types of definitions.
            queries.global_ctxt().unwrap().take().enter(|tcx| {
                let gen = gen::FormalityGen::new(tcx);
                gen.generate(output_format, expect_failure)
            })
        })
    }))
}

/// Runs racket to evaluate the given expression.
/// Returns true if there is no output on stderr, else false.
pub fn run_racket(expr: &str) -> io::Result<bool> {
    let output = Command::new("racket")
        .args(["-l", "errortrace", "-l", "racket/base", "-e", expr])
        .output()?;

    io::stderr().write_all(&output.stderr)?;
    Ok(output.stderr.is_empty())
}
