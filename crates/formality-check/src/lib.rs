#![allow(dead_code)]

use std::{collections::VecDeque, fmt::Debug};

use anyhow::bail;
use formality_core::Set;
use formality_prove::{is_definitely_not_proveable, Decls, Env};
use formality_rust::{
    grammar::{Crate, CrateItem, Program, Test, TestBoundData},
    prove::ToWcs,
};
use formality_types::grammar::{Fallible, Wcs};

mod mini_rust_check;

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
    let decls = program.to_prove_decls();
    Check {
        program,
        decls: &decls,
    }
    .check()
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
        // Collect Fn item from current crate.
        let all_fn: Vec<CrateItem> = c
            .items
            .clone()
            .into_iter()
            .filter(|item| matches!(item, CrateItem::Fn(_)))
            .collect();

        self.check_for_duplicate_items()?;

        for item in items {
            self.check_crate_item(item, &all_fn)?;
        }

        self.check_coherence(c)?;

        Ok(())
    }

    fn check_for_duplicate_items(&self) -> Fallible<()> {
        let Program { crates } = &self.program;
        for c in crates.iter() {
            let mut items = Set::new();
            let mut traits = Set::new();
            let mut functions = Set::new();
            for item in c.items.iter() {
                match item {
                    CrateItem::Struct(s) => {
                        if !items.insert(&s.id) {
                            bail!("the item name `{:?}` is defined multiple times", s.id);
                        }
                    }
                    CrateItem::Enum(e) => {
                        if !items.insert(&e.id) {
                            bail!("the item name `{:?}` is defined multiple times", e.id);
                        }
                    }
                    CrateItem::Trait(t) => {
                        if !traits.insert(&t.id) {
                            bail!("the trait name `{:?}` is defined multiple times", t.id);
                        }
                    }
                    CrateItem::Fn(f) => {
                        if !functions.insert(&f.id) {
                            bail!("the function name `{:?}` is defined multiple times", f.id);
                        }
                    }
                    CrateItem::TraitImpl(_) | CrateItem::NegTraitImpl(_) | CrateItem::Test(_) => {}
                }
            }
        }

        Ok(())
    }

    fn check_crate_item(&self, c: &CrateItem, all_fn: &Vec<CrateItem>) -> Fallible<()> {
        match c {
            CrateItem::Trait(v) => self.check_trait(v, all_fn),
            CrateItem::TraitImpl(v) => self.check_trait_impl(v, all_fn),
            CrateItem::Struct(s) => self.check_adt(&s.to_adt()),
            CrateItem::Enum(e) => self.check_adt(&e.to_adt()),
            CrateItem::Fn(f) => self.check_free_fn(f, all_fn),
            CrateItem::NegTraitImpl(i) => self.check_neg_trait_impl(i),
            CrateItem::Test(t) => self.check_test(t),
        }
    }

    fn check_test(&self, test: &Test) -> Fallible<()> {
        let mut env = Env::default();
        let TestBoundData { assumptions, goals } = env.instantiate_universally(&test.binder);
        self.prove_goal(&env, assumptions, goals)
    }

    fn prove_goal(
        &self,
        env: &Env,
        assumptions: impl ToWcs,
        goal: impl ToWcs + Debug,
    ) -> Fallible<()> {
        let goal: Wcs = goal.to_wcs();
        let assumptions: Wcs = assumptions.to_wcs();

        assert!(env.only_universal_variables());
        assert!(env.encloses((&assumptions, &goal)));

        let cs = formality_prove::prove(self.decls, env, &assumptions, &goal);
        let cs = cs.into_set()?;
        if cs.iter().any(|c| c.unconditionally_true()) {
            return Ok(());
        }

        bail!("failed to prove {goal:?} given {assumptions:?}, got {cs:?}")
    }

    #[tracing::instrument(level = "Debug", skip(self, assumptions, goal))]
    fn prove_not_goal(&self, env: &Env, assumptions: impl ToWcs, goal: impl ToWcs) -> Fallible<()> {
        let goal: Wcs = goal.to_wcs();
        let assumptions: Wcs = assumptions.to_wcs();

        assert!(env.only_universal_variables());
        assert!(env.encloses((&assumptions, &goal)));

        let cs = is_definitely_not_proveable(
            env,
            &assumptions,
            goal.clone(),
            |env, assumptions, goal| formality_prove::prove(self.decls, env, &assumptions, &goal),
        );
        let cs = cs.into_set()?;
        if cs.iter().any(|c| c.unconditionally_true()) {
            return Ok(());
        }

        bail!("failed to prove {goal:?} given {assumptions:?}, got {cs:?}")
    }
}
