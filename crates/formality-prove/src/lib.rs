// Defines the language used by derive(term) and friends.
use formality_types::rust::FormalityLang;

mod db;
mod decls;
mod prove;

pub use decls::*;
pub use prove::prove;
pub use prove::Constraints;
pub use prove::Env;

#[cfg(test)]
mod test;

pub mod test_util;
