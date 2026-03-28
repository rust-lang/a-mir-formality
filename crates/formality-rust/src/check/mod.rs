#![allow(dead_code)]

use std::{collections::VecDeque, fmt::Debug};

use crate::prove::prove::{is_definitely_not_proveable, Constraints, Env, Program};
use crate::rust::Visit;
use crate::{
    grammar::{Crate, CrateId, CrateItem, Crates, Fallible, Test, TestBoundData, Wcs},
    prove::ToWcs,
};
use anyhow::{anyhow, bail};
use formality_core::{judgment::ProofTree, ProvenSet, Set};

use adts::check_adt;
use coherence::check_coherence;
use fns::check_free_fn;
use impls::{check_neg_trait_impl, check_trait_impl};
use traits::check_trait;

pub mod borrow_check;

/// Check all crates in the program. The crates must be in dependency order
/// such that any prefix of the crates is a complete program.
pub fn check_all_crates(all_crates: &Crates) -> Fallible<ProofTree> {
    let Crates { crates } = all_crates;
    let mut crates: VecDeque<_> = crates.iter().cloned().collect();

    let mut proof_tree = ProofTree::new("check_all_crates", None, vec![]);
    let mut prefix_crates = Crates { crates: vec![] };
    while let Some(c) = crates.pop_front() {
        prefix_crates.crates.push(c);
        proof_tree
            .children
            .push(check_current_crate(&prefix_crates)?);
    }

    Ok(proof_tree)
}

/// Checks the current crate in the program, assuming all other crates are valid.
fn check_current_crate(crates: &Crates) -> Fallible<ProofTree> {
    let program = crates.to_prove_decls();
    check(&program)
}

mod adts;
mod coherence;
mod fns;
mod impls;
mod traits;
mod where_clauses;

fn check(program: &Program) -> Fallible<ProofTree> {
    let Crates { crates } = program.program();
    if let Some(current_crate) = crates.last() {
        check_crate(program, current_crate)
    } else {
        Ok(ProofTree::leaf("check (no crates)"))
    }
}

fn check_crate(program: &Program, c: &Crate) -> Fallible<ProofTree> {
    let Crate { id, items } = c;
    let mut proof_tree = ProofTree::new(format!("check_current_crate({id:?})"), None, vec![]);

    check_for_duplicate_items(program)?;

    for item in items {
        proof_tree
            .children
            .push(check_crate_item(program, item, &id)?);
    }

    proof_tree.children.push(check_coherence(program, c)?);

    Ok(proof_tree)
}

fn check_for_duplicate_items(program: &Program) -> Fallible<()> {
    let Crates { crates } = program.program();
    for c in crates.iter() {
        let mut items = Set::new();
        let mut traits = Set::new();
        let mut functions = Set::new();
        for item in c.items.iter() {
            match item {
                CrateItem::AdtItem(s) => {
                    if !items.insert(s.name()) {
                        bail!("the item name `{:?}` is defined multiple times", s.name());
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

fn check_crate_item(program: &Program, c: &CrateItem, crate_id: &CrateId) -> Fallible<ProofTree> {
    match c {
        CrateItem::Trait(v) => check_trait(program, v, crate_id),
        CrateItem::TraitImpl(v) => check_trait_impl(program, v, crate_id),
        CrateItem::AdtItem(s) => check_adt(program, &s.to_adt()),
        CrateItem::Fn(f) => check_free_fn(program, f, crate_id),
        CrateItem::NegTraitImpl(i) => check_neg_trait_impl(program, i),
        CrateItem::Test(t) => check_test(program, t),
        CrateItem::FeatureGate(_feature_gate) => {
            // FIXME(#212): reject duplicate feature gates within a crate
            Ok(ProofTree::leaf("feature gates are OK with me!"))
        }
    }
}

fn check_test(program: &Program, test: &Test) -> Fallible<ProofTree> {
    let mut env = Env::default();
    let TestBoundData { assumptions, goals } = env.instantiate_universally(&test.binder);
    prove_goal(program, &env, assumptions, goals)
}

fn prove_goal(
    program: &Program,
    env: &Env,
    assumptions: impl ToWcs,
    goal: impl ToWcs + Debug,
) -> Fallible<ProofTree> {
    let goal: Wcs = goal.to_wcs();
    prove_judgment(
        program,
        env,
        assumptions,
        goal.to_wcs(),
        crate::prove::prove::prove,
    )
}

fn prove_judgment<G>(
    program: &Program,
    env: &Env,
    assumptions: impl ToWcs,
    goal: G,
    judgment_fn: impl FnOnce(Program, Env, Wcs, G) -> ProvenSet<Constraints>,
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
        program.clone(),
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

#[tracing::instrument(level = "Debug", skip(program, assumptions, goal))]
fn prove_not_goal(
    program: &Program,
    env: &Env,
    assumptions: impl ToWcs,
    goal: impl ToWcs,
) -> Fallible<ProofTree> {
    let goal: Wcs = goal.to_wcs();
    let assumptions: Wcs = assumptions.to_wcs();

    assert!(env.only_universal_variables());
    assert!(env.encloses((&assumptions, &goal)));

    let cs = is_definitely_not_proveable(env, &assumptions, &goal, |env, assumptions, goal| {
        crate::prove::prove::prove(program, env, &assumptions, &goal)
    });
    let cs = cs.into_map()?;
    if let Some((_, proof_tree)) = cs.iter().find(|(c, _)| c.unconditionally_true()) {
        return Ok(proof_tree.clone());
    }

    let constraints: Vec<_> = cs.keys().collect();
    bail!("failed to prove {goal:?} given {assumptions:?}, got {constraints:?}")
}
