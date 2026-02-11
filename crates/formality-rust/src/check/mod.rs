#![allow(dead_code)]

use std::{collections::VecDeque, fmt::Debug};

use crate::prove::prove::{is_definitely_not_proveable, Constraints, Decls, Env};
use crate::rust::Visit;
use crate::{
    grammar::{Crate, CrateId, CrateItem, Fallible, Program, Test, TestBoundData, Wcs},
    prove::ToWcs,
};
use anyhow::{anyhow, bail};
use formality_core::{judgment::ProofTree, ProvenSet, Set};

mod borrow_check;
mod mini_rust_check;

/// Check all crates in the program. The crates must be in dependency order
/// such that any prefix of the crates is a complete program.
pub fn check_all_crates(program: &Program) -> Fallible<ProofTree> {
    let Program { crates } = program;
    let mut crates: VecDeque<_> = crates.iter().cloned().collect();

    let mut proof_tree = ProofTree::new("check_all_crates", None, vec![]);
    let mut prefix_program = Program { crates: vec![] };
    while let Some(c) = crates.pop_front() {
        prefix_program.crates.push(c);
        proof_tree
            .children
            .push(check_current_crate(&prefix_program)?);
    }

    Ok(proof_tree)
}

/// Checks the current crate in the program, assuming all other crates are valid.
fn check_current_crate(program: &Program) -> Fallible<ProofTree> {
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
    fn check(&self) -> Fallible<ProofTree> {
        let Program { crates } = &self.program;
        if let Some(current_crate) = crates.last() {
            self.check_current_crate(current_crate)
        } else {
            Ok(ProofTree::leaf("check (no crates)"))
        }
    }

    fn check_current_crate(&self, c: &Crate) -> Fallible<ProofTree> {
        let Crate { id, items } = c;
        let mut proof_tree = ProofTree::new(format!("check_current_crate({id:?})"), None, vec![]);

        self.check_for_duplicate_items()?;

        for item in items {
            proof_tree.children.push(self.check_crate_item(item, &id)?);
        }

        proof_tree.children.push(self.check_coherence(c)?);

        Ok(proof_tree)
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
                    CrateItem::FeatureGate(_) => {}
                }
            }
        }

        Ok(())
    }

    fn check_crate_item(&self, c: &CrateItem, crate_id: &CrateId) -> Fallible<ProofTree> {
        match c {
            CrateItem::Trait(v) => self.check_trait(v, crate_id),
            CrateItem::TraitImpl(v) => self.check_trait_impl(v, crate_id),
            CrateItem::Struct(s) => self.check_adt(&s.to_adt()),
            CrateItem::Enum(e) => self.check_adt(&e.to_adt()),
            CrateItem::Fn(f) => self.check_free_fn(f, crate_id),
            CrateItem::NegTraitImpl(i) => self.check_neg_trait_impl(i),
            CrateItem::Test(t) => self.check_test(t),
            CrateItem::FeatureGate(_feature_gate) => {
                // FIXME(#212): reject duplicate feature gates within a crate
                Ok(ProofTree::leaf("feature gates are OK with me!"))
            }
        }
    }

    fn check_test(&self, test: &Test) -> Fallible<ProofTree> {
        let mut env = Env::default();
        let TestBoundData { assumptions, goals } = env.instantiate_universally(&test.binder);
        self.prove_goal(&env, assumptions, goals)
    }

    fn prove_goal(
        &self,
        env: &Env,
        assumptions: impl ToWcs,
        goal: impl ToWcs + Debug,
    ) -> Fallible<ProofTree> {
        let goal: Wcs = goal.to_wcs();
        self.prove_judgment(env, assumptions, goal.to_wcs(), crate::prove::prove::prove)
    }

    fn prove_judgment<G>(
        &self,
        env: &Env,
        assumptions: impl ToWcs,
        goal: G,
        judgment_fn: impl FnOnce(Decls, Env, Wcs, G) -> ProvenSet<Constraints>,
    ) -> Fallible<ProofTree>
    where
        G: Debug + Visit + Clone,
    {
        let assumptions: Wcs = assumptions.to_wcs();

        assert!(
            env.only_universal_variables(),
            "env contains existential variables `{env:?}`"
        );
        assert!(env.encloses((&assumptions, &goal)));

        let cs = judgment_fn(
            self.decls.clone(),
            env.clone(),
            assumptions.clone(),
            goal.clone(),
        );
        let cs = cs.into_map()?;
        cs.iter()
            .find_map(|(c, proof_tree)| c.unconditionally_true().then_some(proof_tree))
            .cloned()
            .ok_or_else(|| {
                let constraints: Vec<_> = cs.keys().collect();
                anyhow!("failed to prove `{goal:?}` given `{assumptions:?}`: got {constraints:?}")
            })
    }

    #[tracing::instrument(level = "Debug", skip(self, assumptions, goal))]
    fn prove_not_goal(
        &self,
        env: &Env,
        assumptions: impl ToWcs,
        goal: impl ToWcs,
    ) -> Fallible<ProofTree> {
        let goal: Wcs = goal.to_wcs();
        let assumptions: Wcs = assumptions.to_wcs();

        assert!(env.only_universal_variables());
        assert!(env.encloses((&assumptions, &goal)));

        let cs = is_definitely_not_proveable(
            env,
            &assumptions,
            goal.clone(),
            |env, assumptions, goal| {
                crate::prove::prove::prove(self.decls, env, &assumptions, &goal)
            },
        );
        let cs = cs.into_map()?;
        if let Some((_, proof_tree)) = cs.iter().find(|(c, _)| c.unconditionally_true()) {
            return Ok(proof_tree.clone());
        }

        let constraints: Vec<_> = cs.keys().collect();
        bail!("failed to prove {goal:?} given {assumptions:?}, got {constraints:?}")
    }
}
