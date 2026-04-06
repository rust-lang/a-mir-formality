use crate::grammar::{AssociatedTy, Trait, TraitBoundData, TraitItem};
use crate::grammar::{CrateId, Fallible};
use crate::prove::prove::{Env, Program};
use anyhow::bail;
use formality_core::{judgment::ProofTree, judgment_fn, Set};

judgment_fn! {
    pub(super) fn check_trait(
        program: Program,
        env: Env,
        t: Trait,
        crate_id: CrateId,
    ) => () {
        debug(program, t, crate_id)

        (
            (let Trait { safety: _, id: _, binder } = &t)
            (let (new_env, TraitBoundData { where_clauses, trait_items }) = env.instantiate_universally(&binder.explicit_binder))
            (check_trait_items_have_unique_names(&trait_items) => ())
            (super::where_clauses::prove_where_clauses_well_formed(&program, &new_env, &where_clauses, &where_clauses) => ())
            ------------------------------------------------------------ ("check trait")
            (check_trait(program, env, t, crate_id) => ())
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
