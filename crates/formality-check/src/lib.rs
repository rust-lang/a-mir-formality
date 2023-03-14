#![allow(dead_code)]

use std::collections::VecDeque;

use anyhow::bail;
use formality_prove::{Decls, Env};
use formality_rust::{
    grammar::{Crate, CrateItem, Program, WhereClause},
    prove::ToWcs,
};
use formality_types::{
    cast::Upcast,
    grammar::{Fallible, Wcs},
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
fn check_current_crate(_program: &Program) -> Fallible<()> {
    todo!()
}

mod adts;
mod coherence;
mod fns;
mod impls;
mod traits;
mod where_clauses;

struct Check<'p> {
    program: &'p Program,
    decls: &'p Decls,
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
            CrateItem::Trait(v) => self.check_trait(v),
            CrateItem::TraitImpl(v) => self.check_trait_impl(v),
            CrateItem::Struct(s) => self.check_adt(&s.to_adt()),
            CrateItem::Enum(e) => self.check_adt(&e.to_adt()),
            CrateItem::Fn(f) => self.check_free_fn(f),
        }
    }

    fn prove_goal(
        &self,
        env: &Env,
        assumptions: impl Upcast<Vec<WhereClause>>,
        goal: impl ToWcs,
    ) -> Fallible<()> {
        let goal: Wcs = goal.to_wcs();
        let assumptions: Vec<WhereClause> = assumptions.upcast();

        assert!(env.only_universal_variables());
        assert!(env.encloses((&assumptions, &goal)));

        let cs = formality_prove::prove(self.decls, env, assumptions.to_wcs(), &goal);
        if cs.iter().any(|c| c.unconditionally_true()) {
            return Ok(());
        }

        bail!("failed to prove {goal:?} from {assumptions:?}, got {cs:?}")
    }
}
