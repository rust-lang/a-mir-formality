use formality_types::{
    grammar::{Universe, Variable},
    term::Term,
};

use super::ConstraintSet;

/// Returns the subset of `cs` that reference terms visible from `universe`.
pub fn constraints_visible_from_universe(
    universe: Universe,
    mut cs: ConstraintSet,
) -> ConstraintSet {
    cs.retain(|c| visible_from_universe(c, universe));
    cs
}

fn visible_from_universe<T: Term>(t: &T, u: Universe) -> bool {
    t.free_variables().iter().all(|fv| match fv {
        Variable::PlaceholderVar(p) => p.universe <= u,
        Variable::InferenceVar(_) => false,
        Variable::BoundVar(_) => false,
    })
}
