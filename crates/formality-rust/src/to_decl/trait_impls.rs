use crate::grammar::{self as rust, ImplItem};
use formality_decl::grammar as decl;
use formality_types::{
    cast::Upcast,
    grammar::{Binder, Fallible, TraitRef},
    seq,
};

impl rust::TraitImpl {
    pub(super) fn to_decl(&self) -> Fallible<decl::TraitImpl> {
        let rust::TraitImpl { binder } = self;

        let (
            names,
            rust::TraitImplBoundData {
                trait_id,
                self_ty,
                trait_parameters,
                where_clauses,
                impl_items,
            },
        ) = binder.open();

        let trait_ref = TraitRef::new(&trait_id, seq![self_ty.upcast(), ..trait_parameters]);

        Ok(decl::TraitImpl {
            binder: Binder::new(
                &names,
                decl::TraitImplBoundData {
                    trait_ref,
                    where_clauses: where_clauses
                        .iter()
                        .map(|w| w.to_decl())
                        .collect::<Fallible<_>>()?,
                    impl_items: impl_items
                        .iter()
                        .map(|i| i.to_decl())
                        .collect::<Fallible<_>>()?,
                },
            ),
        })
    }
}

impl ImplItem {
    fn to_decl(&self) -> Fallible<decl::ImplItem> {
        match self {
            ImplItem::Fn(v) => Ok(decl::ImplItem::Fn(v.to_decl()?)),
            ImplItem::AssociatedTyValue(v) => Ok(decl::ImplItem::AssociatedTyValue(v.to_decl()?)),
        }
    }
}
