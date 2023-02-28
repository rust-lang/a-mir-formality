use formality_macros::term;
use formality_types::{
    cast::Upcast,
    collections::{Set, SetExt},
    grammar::{Universe, WcData, Wcs},
    judgment,
    judgment::Judgment,
    set,
    term::Term,
};

use crate::{program::Program, prove::prove_apr::prove_apr};

use super::ConstraintSet;

pub fn forall(universe: Universe, c: ConstraintSet) -> Set<ConstraintSet> {
    ForAll(universe, c).apply()
}

#[term]
struct ForAll(Universe, ConstraintSet);

judgment! {
    (ForAll => ConstraintSet)

    (
        (if let None = cs.split_first())
        --- ("empty")
        (ForAll(u, cs) => cs)
    )

    (
        (if let Some((c0, cs1)) = cs0.split_first())
        
        --- ("empty")
        (ForAll(u, cs0) => cs)
    )
}
