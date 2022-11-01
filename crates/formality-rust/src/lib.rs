use formality_infer::Env;
use formality_logic::{prove_universal_goal, UniversalGoalResult};
use formality_types::{cast::Upcast, db::Db, derive_links, grammar::Fallible, parse::term};

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

pub fn test_can_prove_where_clause(
    program_text: &str,
    where_clause: &str,
) -> Fallible<UniversalGoalResult> {
    formality_core::with_tracing_logs(|| -> Fallible<UniversalGoalResult> {
        let rust_program: grammar::Program = term(program_text);
        let decl_program = rust_program.to_decl()?;
        let db = Db::new(decl_program);
        let env = Env::default();
        let goal: grammar::WhereClause = term(where_clause);
        let predicate = goal.to_decl()?;
        Ok(prove_universal_goal(&db, &env, &[], &predicate.upcast()))
    })
}
