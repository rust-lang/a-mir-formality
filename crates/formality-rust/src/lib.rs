use formality_types::{derive_links, grammar::Fallible};

pub mod grammar;
mod test;
mod to_decl;
mod trait_binder;

pub fn check_program(program: &grammar::Program) -> Fallible<()> {
    let decl_program = program.to_decl()?;
    formality_check::check_program(&decl_program)?;
    Ok(())
}
