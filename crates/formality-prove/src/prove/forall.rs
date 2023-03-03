use formality_types::{
    grammar::{Binder, Universe, Variable},
    visit::Visit,
};

use super::constraints::Constraints;

/// Returns the subset of `cs` that reference terms visible from `universe`.
pub fn constrains_placeholder_in_universe(universe: Universe, cs: &Binder<Constraints>) -> bool {
    cs.free_variables().iter().any(|fv| match fv {
        Variable::PlaceholderVar(pv) => pv.universe >= universe,
        Variable::InferenceVar(_) => false,
        Variable::BoundVar(_) => false,
    })
}
