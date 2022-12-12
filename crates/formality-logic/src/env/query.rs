use formality_macros::term;
use formality_types::grammar::{Goal, Universe};

use super::Env;

mod querify;
mod test;

#[term]
pub struct Query {
    pub env: Env,
    pub goal: Goal,
}

#[term]
pub struct UniverseMap {
    pub universes: Vec<(Universe, Universe)>,
}

pub use querify::querify;
