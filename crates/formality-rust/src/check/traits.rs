use crate::grammar::{
    AssociatedTy, AssociatedTyBoundData, Fn, Trait, TraitBoundData, TraitItem, WhereClause,
};
use crate::grammar::{CrateId, Fallible};
use crate::prove::prove::{Env, Program};
use anyhow::bail;
use fn_error_context::context;
use formality_core::{judgment::ProofTree, Set};

#[context("check_trait({:?})", t.id)]
pub(super) fn check_trait(program: &Program, t: &Trait, crate_id: &CrateId) -> Fallible<ProofTree> {
    let mut proof_tree = ProofTree::leaf("check_trait");

    let Trait {
        safety: _,
        id: _,
        binder,
    } = t;
    let mut env = Env::default();

    let TraitBoundData {
        where_clauses,
        trait_items,
    } = env.instantiate_universally(&binder.explicit_binder);

    proof_tree
        .children
        .push(check_trait_items_have_unique_names(&trait_items)?);

    proof_tree
        .children
        .push(super::where_clauses::prove_where_clauses_well_formed(
            program,
            &env,
            &where_clauses,
            &where_clauses,
        )?);

    for trait_item in &trait_items {
        proof_tree.children.push(check_trait_item(
            program,
            &env,
            &where_clauses,
            trait_item,
            crate_id,
        )?);
    }

    Ok(proof_tree)
}

fn check_trait_items_have_unique_names(trait_items: &[TraitItem]) -> Fallible<ProofTree> {
    let mut functions = Set::new();
    let mut associated_types = Set::new();
    for trait_item in trait_items {
        match trait_item {
            TraitItem::Fn(f) => {
                if !functions.insert(&f.id) {
                    bail!("the function name `{:?}` is defined multiple times", f.id);
                }
            }
            TraitItem::AssociatedTy(associated_ty) => {
                let AssociatedTy { id, .. } = associated_ty;
                if !associated_types.insert(id) {
                    bail!(
                        "the associated type name `{:?}` is defined multiple times",
                        id
                    );
                }
            }
        }
    }
    Ok(ProofTree::leaf(format!(
        "check_trait_items_have_unique_names"
    )))
}

fn check_trait_item(
    program: &Program,
    env: &Env,
    where_clauses: &[WhereClause],
    trait_item: &TraitItem,
    crate_id: &CrateId,
) -> Fallible<ProofTree> {
    match trait_item {
        TraitItem::Fn(v) => check_fn_in_trait(program, env, where_clauses, v, crate_id),
        TraitItem::AssociatedTy(v) => check_associated_ty(program, env, where_clauses, v),
    }
}

fn check_fn_in_trait(
    program: &Program,
    env: &Env,
    where_clauses: &[WhereClause],
    f: &Fn,
    crate_id: &CrateId,
) -> Fallible<ProofTree> {
    Ok(super::fns::check_fn(program, env, where_clauses, f, crate_id).check_proven()?)
}

fn check_associated_ty(
    program: &Program,
    trait_env: &Env,
    trait_where_clauses: &[WhereClause],
    associated_ty: &AssociatedTy,
) -> Fallible<ProofTree> {
    let mut env = trait_env.clone();

    let AssociatedTy { id, binder } = associated_ty;
    let AssociatedTyBoundData {
        ensures: _,
        where_clauses,
    } = env.instantiate_universally(binder);

    let proof_tree = super::where_clauses::prove_where_clauses_well_formed(
        program,
        &env,
        (trait_where_clauses, &where_clauses),
        &where_clauses,
    )?;

    // FIXME(#228) Do we prove ensures WF? And what do we assume when we do so?

    Ok(ProofTree::new(
        format!("check_associated_ty({id:?})"),
        None,
        vec![proof_tree],
    ))
}
