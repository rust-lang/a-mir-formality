use formality_types::{derive_links, grammar::Fallible, parse::term};

pub mod grammar;
mod test;
mod to_decl;
mod trait_binder;

pub fn check_program(program: &grammar::Program) -> Fallible<()> {
    let decl_program = program.to_decl()?;
    formality_check::check_program(&decl_program)?;
    Ok(())
}

pub fn test_program_ok(program_text: &str) -> Fallible<()> {
    formality_core::with_tracing_logs(|| check_program(&term(program_text)))
}
