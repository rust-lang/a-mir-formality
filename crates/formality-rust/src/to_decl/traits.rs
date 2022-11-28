use crate::grammar::{self as rust};
use formality_decl::grammar as decl;
use formality_types::{
    cast::Upcast,
    grammar::{Binder, Fallible},
};

impl rust::Trait {
    pub(super) fn to_decl(&self) -> Fallible<decl::Trait> {
        let rust::Trait { id, binder } = self;
        let (
            names,
            rust::TraitBoundData {
                where_clauses,
                trait_items,
            },
        ) = binder.open();

        Ok(decl::Trait {
            id: id.clone(),
            binder: Binder::new(
                &names,
                decl::TraitBoundData {
                    where_clauses: where_clauses
                        .iter()
                        .map(|wc| wc.to_decl())
                        .collect::<Fallible<_>>()?,
                    trait_items: trait_items
                        .iter()
                        .map(|trait_item| trait_item.to_decl())
                        .collect::<Fallible<_>>()?,
                },
            ),
        })
    }
}

impl rust::TraitItem {
    fn to_decl(&self) -> Fallible<decl::TraitItem> {
        match self {
            rust::TraitItem::Fn(v) => Ok(v.to_decl()?.upcast()),
            rust::TraitItem::AssociatedTy(v) => Ok(v.to_decl()?.upcast()),
        }
    }
}
