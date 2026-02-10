// ANCHOR: use_rust_language
// Defines the language used by derive(term) and friends.
use types::rust::FormalityLang;
// ANCHOR_END: use_rust_language

pub mod check;
pub mod grammar;
pub mod prove;
mod test;
mod trait_binder;
pub mod types;
