use formality_types::{
    grammar::{Binder, InferenceVar, PlaceholderVar, VarIndex, Variable},
    term::Term,
    visit::Visit,
};

pub fn existential_substitution(
    binder: &Binder<impl Term>,
    fresh_in: impl Visit,
) -> Vec<InferenceVar> {
    // We want to avoid any names that appear either in the binder term
    // or the context `fresh_in`, so create a tuple.
    let context = (binder, fresh_in);

    // Our new existential variables can name anything appearing in the context.
    let universe = context.max_universe();

    // We want to ensure every existential variable has a unique index,
    // so find the starting index that makes this different from every
    // existential variable in `fresh_in`.
    let start = context
        .free_variables()
        .into_iter()
        .map(|v| match v {
            Variable::PlaceholderVar(_) => 0,
            Variable::InferenceVar(v) => v.var_index.index + 1,
            Variable::BoundVar(_) => 0,
        })
        .max()
        .unwrap_or(0);

    binder
        .kinds()
        .iter()
        .zip(start..)
        .map(|(&kind, index)| InferenceVar {
            kind,
            var_index: VarIndex { index },
        })
        .collect()
}

pub fn universal_substitution(
    binder: &Binder<impl Term>,
    fresh_in: impl Visit,
) -> Vec<PlaceholderVar> {
    // We want to avoid any names that appear either in the binder term
    // or the context `fresh_in`, so create a tuple.
    let context = (binder, fresh_in);

    // Find the maximum universe in our context, and then add one.
    // This will be the universe for our new placeholders.
    let universe = context.max_universe().next();

    // New placeholders.
    binder
        .kinds()
        .iter()
        .zip(0..)
        .map(|(&kind, index)| PlaceholderVar {
            kind,
            universe,
            var_index: VarIndex { index },
        })
        .collect()
}
