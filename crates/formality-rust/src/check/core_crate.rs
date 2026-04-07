use formality_core::term;

use crate::grammar::{Crate, CrateId, CrateItem};

mod field_projections;

#[term($*items)]
struct LangItems {
    items: Vec<CrateItem>,
}

pub fn krate() -> Crate {
    Crate {
        id: CrateId::new("core"),
        items: [field_projections::lang_items()]
            .into_iter()
            .flat_map(|items| items.items)
            .collect(),
    }
}
