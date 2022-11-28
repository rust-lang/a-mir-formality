use crate::grammar as rust;
use formality_decl::grammar as decl;
use formality_types::{cast::Upcast, grammar::Fallible};

mod associated_tys;
mod enums;
mod fields;
mod fns;
mod structs;
mod trait_impls;
mod traits;
mod variants;
mod where_clauses_and_bounds;

impl rust::Program {
    pub fn to_decl(&self) -> Fallible<decl::Program> {
        let rust::Program { crates } = self;
        Ok(decl::Program {
            crates: crates
                .iter()
                .map(|c| c.to_decl())
                .collect::<Fallible<_>>()?,
        })
    }
}

impl rust::Crate {
    fn to_decl(&self) -> Fallible<decl::Crate> {
        let rust::Crate { id, items } = self;
        Ok(decl::Crate {
            id: id.clone(),
            items: items.iter().map(|c| c.to_decl()).collect::<Fallible<_>>()?,
        })
    }
}

impl rust::CrateItem {
    fn to_decl(&self) -> Fallible<decl::CrateItem> {
        match self {
            rust::CrateItem::Struct(v) => Ok(v.to_decl()?.upcast()),
            rust::CrateItem::Enum(v) => Ok(v.to_decl()?.upcast()),
            rust::CrateItem::Trait(v) => Ok(v.to_decl()?.upcast()),
            rust::CrateItem::TraitImpl(v) => Ok(v.to_decl()?.upcast()),
        }
    }
}
