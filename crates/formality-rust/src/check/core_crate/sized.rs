use crate::parse_term;

use super::LangItems;

pub fn lang_items() -> LangItems {
    parse_term! {
        // No impls: `prove_sized` has built-in rules for `Sized`.
        trait Sized {}
    }
}
