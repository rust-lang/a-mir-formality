#![allow(dead_code)]

use std::collections::VecDeque;

use anyhow::bail;
use formality_prove::{Decls, Env};
use formality_rust::{
    grammar::{Crate, CrateItem, Program},
    prove::ToWcs,
};
use formality_types::{
    cast::Upcast,
    grammar::{Fallible, Substitution, TraitRef, Wcs},
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
            CrateItem::NegTraitImpl(i) => self.check_neg_trait_impl(i),
        }
    }

    fn prove_goal(&self, env: &Env, assumptions: impl ToWcs, goal: impl ToWcs) -> Fallible<()> {
        let goal: Wcs = goal.to_wcs();
        let assumptions: Wcs = assumptions.to_wcs();

        assert!(env.only_universal_variables());
        assert!(env.encloses((&assumptions, &goal)));

        let cs = formality_prove::prove(self.decls, env, &assumptions, &goal);
        if cs.iter().any(|c| c.unconditionally_true()) {
            return Ok(());
        }

        bail!("failed to prove {goal:?} given {assumptions:?}; got {cs:?}")
    }

    fn prove_is_local_trait_ref(
        &self,
        env: &Env,
        assumptions: impl ToWcs,
        goal: impl Upcast<TraitRef>,
    ) -> Fallible<()> {
        let goal: TraitRef = goal.upcast();
        let assumptions: Wcs = assumptions.to_wcs();

        assert!(env.only_universal_variables());
        assert!(env.encloses((&assumptions, &goal)));

        let cs = formality_prove::prove_is_local_trait_ref(self.decls, env, &assumptions, &goal);
        if cs.iter().any(|c| c.unconditionally_true()) {
            return Ok(());
        }

        bail!("failed to prove {goal:?} is local, given {assumptions:?}; got {cs:?}")
    }

    fn prove_not_goal(&self, env: &Env, assumptions: impl ToWcs, goal: impl ToWcs) -> Fallible<()> {
        let goal: Wcs = goal.to_wcs();
        let assumptions: Wcs = assumptions.to_wcs();

        assert!(env.only_universal_variables());
        assert!(env.encloses((&assumptions, &goal)));

        // Proving `∀X. not(F(X))` is the same as proving: `not(∃X. F(X))`.
        // Therefore, since we have only universal variables, we can change them all to
        // existential and then try to prove. If we get back no solutions, we know that
        // we've proven the negation. (This is called the "negation as failure" property,
        // and it relies on our solver being complete -- i.e., if there is a solution,
        // we'll find it, or at least return ambiguous.)
        let mut existential_env = Env::default().with_coherence_mode(env.is_in_coherence_mode());
        let universal_to_existential: Substitution = env
            .variables()
            .iter()
            .map(|v| {
                assert!(v.is_universal());
                let v1 = existential_env.fresh_existential(v.kind());
                (v, v1)
            })
            .collect();

        let existential_assumptions = universal_to_existential.apply(&assumptions);
        let existential_goal = universal_to_existential.apply(&goal);

        let cs = formality_prove::prove(
            self.decls,
            &existential_env,
            existential_assumptions.to_wcs(),
            &existential_goal,
        );

        if cs.is_empty() {
            return Ok(());
        }

        bail!("failed to disprove {goal:?} given {assumptions:?}, got {cs:?}")
    }
}
