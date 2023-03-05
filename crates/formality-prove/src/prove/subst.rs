use formality_types::{
    fold::Fold,
    grammar::{Binder, InferenceVar, ParameterKind, PlaceholderVar, Universe, VarIndex, Variable},
    term::Term,
    visit::Visit,
};

pub fn fresh_existentials(
    universe: Universe,
    kinds: &[ParameterKind],
    fresh_in: impl Visit,
) -> Vec<InferenceVar> {
    // We want to ensure every existential variable has a unique index,
    // so find the starting index that makes this different from every
    // existential variable in `fresh_in`.
    let start = fresh_in
        .free_variables()
        .into_iter()
        .map(|v| match v {
            Variable::PlaceholderVar(_) => 0,
            Variable::InferenceVar(v) => v.var_index.index + 1,
            Variable::BoundVar(_) => 0,
        })
        .max()
        .unwrap_or(0);

    kinds
        .iter()
        .zip(start..)
        .map(|(&kind, index)| InferenceVar {
            kind,
            universe,
            var_index: VarIndex { index },
        })
        .collect()
}

pub fn fresh_existential(
    universe: Universe,
    kind: ParameterKind,
    fresh_in: impl Visit,
) -> InferenceVar {
    let vars = fresh_existentials(universe, &[kind], fresh_in);
    vars.into_iter().next().unwrap()
}

pub fn existential_substitution(
    binder: &Binder<impl Fold>,
    fresh_in: impl Visit,
) -> Vec<InferenceVar> {
    // Able to name anything in `fresh_in`
    let universe = fresh_in.max_universe();
    fresh_existentials(universe, binder.kinds(), (binder, fresh_in))
}

pub fn universal_substitution(
    binder: &Binder<impl Term>,
    fresh_in: impl Visit,
) -> (Universe, Vec<PlaceholderVar>) {
    // We want to avoid any names that appear either in the binder term
    // or the context `fresh_in`, so create a tuple.
    let context = (binder, fresh_in);

    // Find the maximum universe in our context, and then add one.
    // This will be the universe for our new placeholders.
    let universe = context.max_universe().next();

    // New placeholders.
    let subst = binder
        .kinds()
        .iter()
        .zip(0..)
        .map(|(&kind, index)| PlaceholderVar {
            kind,
            universe,
            var_index: VarIndex { index },
        })
        .collect();

    (universe, subst)
}
