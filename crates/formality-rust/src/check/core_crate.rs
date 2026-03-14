use formality_core::term;

use crate::grammar::{Crate, CrateId, CrateItem};

#[term($*items)]
struct LangItems {
    items: Vec<CrateItem>,
}

pub fn krate() -> Crate {
    Crate {
        id: CrateId::new("core"),
        items: []
            .into_iter()
            .flat_map(|items: LangItems| items.items)
            .collect(),
    }
}
