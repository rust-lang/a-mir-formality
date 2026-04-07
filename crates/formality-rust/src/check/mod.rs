#![allow(dead_code)]

use std::fmt::Debug;

use crate::prove::prove::{is_definitely_not_proveable, Constraints, Env, Program};
use crate::rust::Visit;
use crate::{
    grammar::{Crate, CrateId, CrateItem, Crates, Fallible, Test, TestBoundData, Wcs},
    prove::ToWcs,
};
use anyhow::{anyhow, bail};
use formality_core::{judgment::ProofTree, judgment_fn, ProvenSet, Set};

use adts::check_adt;
use coherence::check_coherence;
use fns::check_free_fn;
use impls::{check_neg_trait_impl, check_trait_impl};
use traits::check_trait;

pub mod borrow_check;

mod adts;
mod coherence;
mod core_crate;
mod fns;
mod impls;
mod traits;
mod where_clauses;

judgment_fn! {
    pub fn check_all_crates(
        crates: Crates,
    ) => () {
        debug(crates)

        (
            // Add the core crate if the first crate isn't called `core`.
            (let crates = {
                let Crates { mut crates } = crates.clone();
                if Some("core") != crates.first().map(|first| &**first.id) {
                    crates.push(core_crate::krate());
                }
                Crates { crates }
            })
            // Check that all crates up to and including crate #i are valid.
            // Crate #i will be considered the "current crate".
            (for_all(i in 0..crates.len())
                (let program = crates.prefix(i).to_prove_decls())
                (check_crate(program, &crates.crates[i]) => ()))
            ------------------------------------------------------------ ("check all prefixes")
            (check_all_crates(crates) => ())
        )
    }
}

judgment_fn! {
    /// Check that the current crate `c` is valid,
    /// given the complete program (which includes dependencies).
    fn check_crate(
        program: Program,
        c: Crate,
    ) => () {
        debug(program, c)

        (
            (check_for_duplicate_items(program) => ())
            (for_all(item in &c.items)
                (check_crate_item(program, item, &c.id) => ()))
            (check_coherence(program, c) => ())
            ------------------------------------------------------------ ("check crate")
            (check_crate(program, Crate { id, items }) => ())
        )
    }
}

fn check_for_duplicate_items(program: &Program) -> Fallible<ProofTree> {
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

    Ok(ProofTree::leaf(
        "check_for_duplicate_items: no duplicates found",
    ))
}

judgment_fn! {
    fn check_crate_item(
        program: Program,
        c: CrateItem,
        crate_id: CrateId,
    ) => () {
        debug(program, c, crate_id)

        (
            (check_trait(program, Env::default(), v, crate_id) => ())
            ------------------------------------------------------------ ("trait")
            (check_crate_item(program, CrateItem::Trait(v), crate_id) => ())
        )

        (
            (check_trait_impl(program, v, crate_id) => ())
            ------------------------------------------------------------ ("trait impl")
            (check_crate_item(program, CrateItem::TraitImpl(v), crate_id) => ())
        )

        (
            (check_adt(program, &s.to_adt()) => ())
            ------------------------------------------------------------ ("adt")
            (check_crate_item(program, CrateItem::AdtItem(s), crate_id) => ())
        )

        (
            (check_free_fn(program, f, crate_id) => ())
            ------------------------------------------------------------ ("free fn")
            (check_crate_item(program, CrateItem::Fn(f), crate_id) => ())
        )

        (
            (check_neg_trait_impl(program, i) => ())
            ------------------------------------------------------------ ("neg trait impl")
            (check_crate_item(program, CrateItem::NegTraitImpl(i), crate_id) => ())
        )

        (
            (check_test(program, t) => ())
            ------------------------------------------------------------ ("test")
            (check_crate_item(program, CrateItem::Test(t), crate_id) => ())
        )

        (
            // FIXME(#212): reject duplicate feature gates within a crate
            ------------------------------------------------------------ ("feature gate")
            (check_crate_item(program, CrateItem::FeatureGate(_feature_gate), crate_id) => ())
        )
    }
}

fn check_test(program: &Program, test: &Test) -> Fallible<ProofTree> {
    let (env, TestBoundData { assumptions, goals }) =
        Env::default().instantiate_universally(&test.binder);
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
