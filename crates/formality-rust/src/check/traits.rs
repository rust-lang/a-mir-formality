use crate::grammar::{AssociatedTy, Trait, TraitBoundData, TraitItem};
use crate::grammar::{CrateId, Fallible};
use formality_core::{judgment_fn, judgment::ProofTree, Set};
use anyhow::bail;
use crate::prove::prove::{Env, Program};

judgment_fn! {
    pub(super) fn check_trait(
        program: Program,
        t: Trait,
        crate_id: CrateId,
    ) => () {
        debug(program, t, crate_id)

        (
            (let Trait { safety: _, id: _, binder } = &t)
            (let (_, TraitBoundData { where_clauses: _, trait_items }) = binder.open())
            (check_trait_items_have_unique_names(&trait_items) => ())
            ------------------------------------------------------------ ("check trait")
            (check_trait(program, t, crate_id) => ())
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
                    bail!("the associated type name `{:?}` is defined multiple times", id);
                }
            }
        }
    }
    Ok(ProofTree::leaf("check_trait_items_have_unique_names"))
}
