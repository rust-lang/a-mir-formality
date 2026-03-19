use crate::grammar::{AdtId, AdtItem, Crate, CrateItem, Fn, Struct, Trait, ValueId};
use crate::grammar::{Fallible, TraitId};
use formality_core::term;

#[term($crates)]
pub struct Program {
    /// List of all crates.
    /// The last crate in the list is the current crate.
    pub crates: Vec<Crate>,
}

impl Program {
    pub fn items_from_all_crates(&self) -> impl Iterator<Item = &CrateItem> {
        self.crates.iter().flat_map(|c| &c.items)
    }

    pub fn trait_named(&self, trait_id: &TraitId) -> Fallible<&Trait> {
        let mut traits: Vec<&Trait> = self
            .items_from_all_crates()
            .filter_map(|crate_item| match crate_item {
                CrateItem::Trait(t) if t.id == *trait_id => Some(t),
                _ => None,
            })
            .collect();
        if traits.is_empty() {
            anyhow::bail!("no trait named `{trait_id:?}`")
        } else if traits.len() > 1 {
            anyhow::bail!("multiple traits named `{trait_id:?}`")
        } else {
            Ok(traits.pop().unwrap())
        }
    }

    pub fn fn_named(&self, fn_id: &ValueId) -> Fallible<&Fn> {
        let mut fns: Vec<&Fn> = self
            .items_from_all_crates()
            .filter_map(|crate_item| match crate_item {
                CrateItem::Fn(f) if f.id == *fn_id => Some(f),
                _ => None,
            })
            .collect();
        if fns.is_empty() {
            anyhow::bail!("no fn named `{fn_id:?}`")
        } else if fns.len() > 1 {
            anyhow::bail!("multiple fns named `{fn_id:?}`")
        } else {
            Ok(fns.pop().unwrap())
        }
    }

    pub fn struct_named(&self, adt_id: &AdtId) -> Fallible<&Struct> {
        let mut structs: Vec<&Struct> = self
            .items_from_all_crates()
            .filter_map(|crate_item| match crate_item {
                CrateItem::AdtItem(AdtItem::Struct(s)) if s.id == *adt_id => Some(s),
                _ => None,
            })
            .collect();
        if structs.is_empty() {
            anyhow::bail!("no struct named `{adt_id:?}`")
        } else if structs.len() > 1 {
            anyhow::bail!("multiple structs named `{adt_id:?}`")
        } else {
            Ok(structs.pop().unwrap())
        }
    }

    pub fn adt_item_named(&self, adt_id: &AdtId) -> Fallible<&AdtItem> {
        let mut adts: Vec<&AdtItem> = self
            .items_from_all_crates()
            .filter_map(|crate_item| match crate_item {
                CrateItem::AdtItem(a) if a.name() == *adt_id => Some(a),
                _ => None,
            })
            .collect();
        if adts.is_empty() {
            anyhow::bail!("no ADT named `{adt_id:?}`")
        } else if adts.len() > 1 {
            anyhow::bail!("multiple ADTs named `{adt_id:?}`")
        } else {
            Ok(adts.pop().unwrap())
        }
    }
}
