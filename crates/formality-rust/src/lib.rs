#![cfg(FIXME)]

use formality_logic::UniversalGoalResult;
use formality_types::derive_links;
use formality_types::{grammar::Fallible, parse::term};

pub mod grammar;
mod test;
mod to_decl;
mod trait_binder;

pub fn check_program(program: &grammar::Program) -> Fallible<()> {
    let decl_program = program.to_decl()?;
    formality_check::check_all_crates(&decl_program)?;
    Ok(())
}

pub fn test_program_ok(program_text: &str) -> Fallible<()> {
    formality_core::with_tracing_logs(|| check_program(&term(program_text)))
}

pub fn test_can_prove_where_clause(
    _program_text: &str,
    _where_clause: &str,
) -> Fallible<UniversalGoalResult> {
    todo!()
}

pub fn test_can_prove_goal(_program_text: &str, _goal: &str) -> Fallible<UniversalGoalResult> {
    todo!()
}
