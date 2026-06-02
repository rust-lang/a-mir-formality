use formality_core::term;

use crate::grammar::{Crate, CrateId, CrateItem};

mod copy;
mod field_projections;

#[term($*items)]
struct LangItems {
    items: Vec<CrateItem>,
}

pub fn krate() -> Crate {
    Crate {
        id: CrateId::new("core"),
        items: [
            copy::lang_items(),
            field_projections::lang_items(), //
        ]
        .into_iter()
        .flat_map(|items| items.items)
        .collect(),
    }
}
