use std::collections::VecDeque;

use anyhow::bail;
use contracts::requires;
use formality_decl::grammar::{Crate, CrateItem, Program};
use formality_logic::Db;
use formality_logic::Env;
use formality_logic::{prove_universal_goal, UniversalGoalResult};
use formality_types::{
    cast::Upcast,
    grammar::{Fallible, Goal, Hypothesis},
    term::Term,
};

/// Check all crates in the program. The crates must be in dependency order
/// such that any prefix of the crates is a complete program.
pub fn check_all_crates(program: &Program) -> Fallible<()> {
    let Program { crates } = program;
    let mut crates: VecDeque<_> = crates.iter().cloned().collect();

    let mut prefix_program = Program { crates: vec![] };
    while let Some(c) = crates.pop_front() {
        prefix_program.crates.push(c);
        check_current_crate(&prefix_program)?;
    }

    Ok(())
}

/// Checks the current crate in the program, assuming all other crates are valid.
fn check_current_crate(program: &Program) -> Fallible<()> {
    let db = Db::new(program.clone());
    Check { program, db }.check()
}

mod adts;
mod coherence;
mod fns;
mod impls;
mod traits;
mod where_clauses;

struct Check<'p> {
    program: &'p Program,
    db: Db,
}

impl Check<'_> {
    fn check(&self) -> Fallible<()> {
        let Program { crates } = &self.program;
        if let Some(current_crate) = crates.last() {
            self.check_current_crate(current_crate)?;
        }
        Ok(())
    }

    fn check_current_crate(&self, c: &Crate) -> Fallible<()> {
        let Crate { id: _, items } = c;

        self.check_for_duplicate_items()?;

        for item in items {
            self.check_crate_item(item)?;
        }

        self.check_coherence(c)?;

        Ok(())
    }

    fn check_for_duplicate_items(&self) -> Fallible<()> {
        // FIXME: check for items with duplicate names, respecting the various Rust rules about namespaces
        Ok(())
    }

    fn check_crate_item(&self, c: &CrateItem) -> Fallible<()> {
        match c {
            CrateItem::Adt(v) => self.check_adt(v),
            CrateItem::Trait(v) => self.check_trait(v),
            CrateItem::TraitImpl(v) => self.check_trait_impl(v),
            CrateItem::Fn(v) => self.check_free_fn(v),
        }
    }

    #[requires(goal.references_only_placeholder_variables())]
    fn prove_goal(
        &self,
        env: &Env,
        assumptions: impl Upcast<Vec<Hypothesis>>,
        goal: impl Term + Upcast<Goal>,
    ) -> Fallible<()> {
        let goal: Goal = goal.upcast();
        let assumptions = assumptions.upcast();
        match prove_universal_goal(&self.db, env, &assumptions, &goal) {
            UniversalGoalResult::Yes => Ok(()),
            UniversalGoalResult::No => bail!("could not prove `{goal:?}` given `{assumptions:#?}`"),
            UniversalGoalResult::Maybe => {
                bail!("could not prove `{goal:?}` (ambiguous) given `{assumptions:#?}`")
            }
        }
    }
}
