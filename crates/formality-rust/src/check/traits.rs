use crate::grammar::{AssociatedTy, AssociatedTyBoundData, Fn, Kinded, Trait, TraitBoundData, TraitItem, Ty, TyData, Wcs};
use crate::grammar::{CrateId, Fallible};
use crate::prove::prove::{Env, Program};
use anyhow::bail;
use formality_core::{judgment::ProofTree, judgment_fn, Set, Upcast};

judgment_fn! {
    pub(super) fn check_trait(
        program: Program,
        env: Env,
        t: Trait,
        crate_id: CrateId,
    ) => () {
        debug(program, t, crate_id)

        (
            (let Trait { safety: _, id: _, binder } = t)
            (let (env, bound_data) = env.instantiate_universally(&binder.explicit_binder))
            (let TraitBoundData { where_clauses, trait_items } = bound_data)
            (check_trait_items_have_unique_names(&trait_items) => ())
            (super::where_clauses::prove_where_clauses_well_formed(program, env, where_clauses, where_clauses) => ())
            (for_all(trait_item in trait_items)
                (check_trait_item(program, env, where_clauses, trait_item, crate_id) => ()))
            ------------------------------------------------------------ ("check trait")
            (check_trait(program, env, t, crate_id) => ())
        )
    }
}

judgment_fn! {
    fn check_trait_item(
        program: Program,
        env: Env,
        where_clauses: Wcs,
        trait_item: TraitItem,
        crate_id: CrateId,
    ) => () {
        debug(program, env, where_clauses, trait_item, crate_id)

        (
            (check_fn_in_trait(program, env, where_clauses, f, crate_id) => ())
            ------------------------------------------------------------ ("fn in trait")
            (check_trait_item(program, env, where_clauses, TraitItem::Fn(f), crate_id) => ())
        )

        (
            (check_associated_ty(program, env, where_clauses, v) => ())
            ------------------------------------------------------------ ("associated ty in trait")
            (check_trait_item(program, env, where_clauses, TraitItem::AssociatedTy(v), crate_id) => ())
        )
    }
}

judgment_fn! {
    fn check_fn_in_trait(
        program: Program,
        env: Env,
        assumptions: Wcs,
        f: Fn,
        crate_id: CrateId,
    ) => () {
        debug(program, env, assumptions, f, crate_id)

        (
            (super::fns::check_fn(program, env, assumptions, f, crate_id) => ())
            ------------------------------------------------------------ ("check fn in trait")
            (check_fn_in_trait(program, env, assumptions, f, crate_id) => ())
        )
    }
}

judgment_fn! {
    fn check_associated_ty(
        program: Program,
        env: Env,
        trait_where_clauses: Wcs,
        associated_ty: AssociatedTy,
    ) => () {
        debug(program, env, trait_where_clauses, associated_ty)

        (
            (let AssociatedTy { id: _, binder } = associated_ty)
            (let (env, bound_data) = env.instantiate_universally(binder))
            (let AssociatedTyBoundData { ensures, where_clauses } = bound_data)
            (let self_ty: Ty = crate::grammar::TyData::Variable(env.variables().last().unwrap().clone()).upcast())
            (let ensures_wcs: Wcs = ensures.iter().map(|e| e.to_wc(&self_ty)).collect())
            (super::where_clauses::prove_where_clauses_well_formed(
                program, env, (trait_where_clauses, where_clauses), where_clauses,
            ) => ())
            (super::prove_goal(program, env, (trait_where_clauses, where_clauses), ensures_wcs) => ())
            ------------------------------------------------------------ ("check associated ty")
            (check_associated_ty(program, env, trait_where_clauses, associated_ty) => ())
        )
    }
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
            TraitItem::AssociatedTy(AssociatedTy { id, .. }) => {
                if !associated_types.insert(id) {
                    bail!(
                        "the associated type name `{:?}` is defined multiple times",
                        id
                    );
                }
            }
        }
    }
    Ok(ProofTree::leaf("check_trait_items_have_unique_names"))
}
