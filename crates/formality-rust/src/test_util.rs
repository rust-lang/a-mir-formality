use rustc_driver::{run_compiler, Callbacks, Compilation};
use rustc_interface::Config;
use rustc_public::CompilerError;
use std::{cell::LazyCell, ops::ControlFlow};

use crate::{pp::PrettyPrinter, rust::try_term};

/// This object controls rustc's behavior.
pub struct RustCompiler {
    file_name: String,
    input: String,
    result: Option<ControlFlow<(), ()>>,
}

impl RustCompiler {
    /// Compiles a single Rust crate from an in‑memory source string.
    ///
    /// The `run` method invokes `rustc_driver` so that the compiler reads
    /// `input` as the contents of a virtual file named `file_name`. No real
    /// filesystem access is required. Additional compiler flags may be
    /// supplied through `args`
    ///
    /// Returns `Ok(())` if the compiler session completed without any errors.
    ///
    /// Returns `Err(CompilerError::Failed)` if a fatal error occurred or the
    /// compiler aborted unexpectedly.
    ///
    /// # Panics
    ///
    /// Panics if the compiler attempts to enter code generation, as this
    /// indicates a bug in the `Callbacks` implementation.
    pub fn run(
        file_name: impl Into<String>,
        input: impl Into<String>,
        args: &[String],
    ) -> Result<(), CompilerError<()>> {
        let mut compiler = RustCompiler {
            file_name: file_name.into(),
            input: input.into(),
            result: None,
        };

        let compiler_result =
            rustc_driver::catch_fatal_errors(|| -> rustc_interface::interface::Result<()> {
                run_compiler(&args, &mut compiler);
                Ok(())
            });

        match (compiler_result, compiler.result.take()) {
            (Ok(Ok(())), Some(ControlFlow::Break(()))) | (Ok(Ok(())), None) => Ok(()),
            (Ok(Ok(())), Some(ControlFlow::Continue(()))) => {
                panic!("Code generation should never happen")
            }
            // Two cases here:
            // - `run` finished normally and returned `Err`
            // - `run` panicked with `FatalErr`
            // You might think that normal compile errors cause the former, and
            // ICEs cause the latter. But some normal compiler errors also cause
            // the latter. So we can't meaningfully distinguish them, and group
            // them together.
            (Ok(Err(_)), _) | (Err(_), _) => Err(CompilerError::Failed),
        }
    }
}

impl Callbacks for RustCompiler {
    fn config(&mut self, config: &mut Config) {
        config.input = rustc_session::config::Input::Str {
            name: rustc_span::FileName::Custom(self.file_name.clone()),
            input: self.input.clone(),
        };
    }

    fn after_analysis(
        &mut self,
        _compiler: &rustc_interface::interface::Compiler,
        _tcx: rustc_middle::ty::TyCtxt,
    ) -> Compilation {
        // We never want to generate an actual binary, so we stop after the analysis
        Compilation::Stop
    }
}

const DEFAULT_ARGS: LazyCell<[String; 6]> = LazyCell::new(|| {
    [
        "--out-dir /tmp/formality/".to_owned(),
        "--crate-type".to_owned(),
        "lib".to_owned(),
        "--allow".to_owned(),
        "warnings".to_owned(),
        "/tmp/test.rs".to_owned(),
    ]
});

#[macro_export]
macro_rules! assert_ok {
    ($input:tt) => {{
        let _ = $crate::test_util::run(
            "dummy-file.rs",
            stringify!($input),
            &*$crate::test_util::DEFAULT_ARGS,
        )
        .unwrap();
    }};
}

#[macro_export]
macro_rules! assert_err {
    ($input:tt) => {{
        let _ = $crate::test_util::run(
            "dummy-file.rs",
            stringify!($input),
            &*$crate::test_util::DEFAULT_ARGS,
        )
        .unwrap_err();
    }};
}

/// Compiles a Formality program into Rust code and invokes `rustc` to
/// type‑check and analyze the generated crate.
///
/// The function expands the Formality `input` into a Rust crate and forwards
/// it, together with the dummy file name `file_name`, to the compiler.
/// Only programs that translate into a single crate are supported.
///
/// # Example
///
/// ```no_run
/// # use formality_rust::test_util::run;
/// let rust_args = vec!["--crate-type=lib".into()];
/// run("virtual.rs", "some_formality_program", &rust_args).unwrap();
/// ```
pub fn run(file_name: &str, input: &str, args: &[String]) -> anyhow::Result<()> {
    let program = try_term(input)?;
    let krates = PrettyPrinter::default().print_crates(&program)?;

    if krates.len() != 1 {
        anyhow::bail!("Only a single crate is supported");
    }

    let krate = &krates[0];

    RustCompiler::run(file_name.to_owned(), krate, args).map_err(|err| anyhow::anyhow!("{err:?}"))
}

#[cfg(test)]
mod test {
    #[test]
    fn blub() {
        assert_ok!([
            crate Foo { fn foo() -> i32 { trusted }}
        ]);

        // Fails, because Bar is not a known type for rustc.
        assert_err!([
            crate Foo {
                fn foo() -> Bar { trusted }
            }
        ]);
    }
}
