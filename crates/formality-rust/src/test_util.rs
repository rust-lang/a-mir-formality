#![cfg(test)]

use rustc_driver::{run_compiler, Callbacks, Compilation};
use rustc_public::CompilerError;
use std::{cell::LazyCell, ops::ControlFlow};

use crate::{grammar::Crates, pp::PrettyPrinter};

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
    fn config(&mut self, config: &mut rustc_interface::Config) {
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

/// Parses the given token tree as Formality crates, generates valid
/// Rust code from them, and then compiles and analyzes that code
/// using `rustc`.
///
/// Asserts that `rustc` can successfully compile the generated code.
///
/// Panics if the input is not a valid Formality program or if compilation
/// fails.
#[macro_export]
macro_rules! assert_rustc_ok {
    ($input:tt) => {{
        let crates = $crate::rust::term(stringify!($input));
        let _ = $crate::test_util::run("dummy-file.rs", &crates, &*$crate::test_util::DEFAULT_ARGS)
            .unwrap();
    }};
}

/// Parses the given token tree as Formality crates, generates valid
/// Rust code from them, and then compiles and analyzes that code
/// using `rustc`.
///
/// Asserts that `rustc` can **not** successfully compile the generated code.
///
/// Panics if the input is not a valid Formality program or if compilation
/// succeeds.
#[macro_export]
macro_rules! assert_rustc_err {
    ($input:tt) => {{
        let crates = $crate::rust::term(stringify!($input));
        let _ = $crate::test_util::run("dummy-file.rs", &crates, &*$crate::test_util::DEFAULT_ARGS)
            .unwrap_err();
    }};
}

/// Parses the given token tree as Formality crates, generates valid
/// Rust code from them, and then compiles and analyzes that code
/// using `rustc`.
///
/// Asserts that Formality and `rustc` arrive at the same conclusion about the
/// program’s validity.
///
/// Panics if the input is not a valid Formality program or if Formality and
/// `rustc` produce differing outcomes
#[macro_export]
macro_rules! assert_rustc_equality {
    ($input:tt) => {{
        let crates = $crate::rust::term(stringify!($input));
        let result = $crate::test_util::formality_rustc_equality(
            "dummy-file.rs",
            crates,
            &*$crate::test_util::DEFAULT_ARGS,
        )
        .unwrap();
        assert!(result);
    }};
}

/// Compiles a Formality program into Rust code and invokes `rustc` to
/// type‑check and analyze the generated crate.
///
/// The function expands the Formality `crates` into a Rust crate and forwards
/// it, together with the dummy file name `file_name`, to the compiler.
/// Only programs that translate into a single crate are supported.
///
/// # Example
///
/// ```no_run
/// # use formality_rust::test_util::run;
/// # use formality_rust::grammar::Crates;
/// let formality = Crates { todo!() };
/// let rust_args = vec!["--crate-type=lib".into()];
/// run("virtual.rs", formality, &rust_args).unwrap();
/// ```
pub fn run(file_name: &str, crates: &Crates, args: &[String]) -> anyhow::Result<()> {
    if crates.crates.len() != 1 {
        anyhow::bail!("Only a single crate is supported");
    }
    let krates = PrettyPrinter::default().print_crates(crates)?;
    RustCompiler::run(file_name.to_owned(), &krates[0], args)
        .map_err(|err| anyhow::anyhow!("{err:?}"))
}

/// Compiles a Formality program into Rust code, invokes `rustc`,
/// and compares rustc’s analysis result with Formality’s analysis result.
///
/// # Example
///
/// ```no_run
/// # use formality_rust::test_util::formality_rustc_equality;
/// # use formality_rust::grammar::Crates;
/// let formality = Crates { todo!() };
/// let rust_args = vec!["--crate-type=lib".into()];
/// assert!(formality_rustc_equality("virtual.rs", formality, &rust_args).unwrap());
/// ```
pub fn formality_rustc_equality(
    file_name: &str,
    crates: Crates,
    args: &[String],
) -> anyhow::Result<bool> {
    use crate::check::check_all_crates;

    let rustc_result = run(file_name, &crates, args);
    let formality_result = check_all_crates(crates).check_proven();

    match (formality_result, rustc_result) {
        (Ok(_), Ok(_)) => Ok(true),
        (Err(_), Err(_)) => Ok(true),
        _ => Ok(false),
    }
}

#[test]
fn test_macros() {
    assert_rustc_ok!([
        crate Foo { fn foo() -> i32 { trusted }}
    ]);

    assert_rustc_err!([
        crate Foo {
            fn foo() -> Bar { trusted }
        }
    ]);

    assert_rustc_equality!([
        crate Foo { fn foo() -> i32 { trusted }}
    ]);

    assert_rustc_equality!([
        crate Foo {
            fn foo() -> Bar { trusted }
        }
    ]);
}
