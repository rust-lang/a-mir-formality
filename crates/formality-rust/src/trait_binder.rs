use std::fmt::Debug;

use formality_types::{
    cast::To,
    derive_links::{Downcast, UpcastFrom},
    fold::Fold,
    grammar::{fresh_bound_var, Binder, KindedVarIndex, ParameterKind},
    parse::{expect_char, Binding, Parse},
    term::Term,
};

use crate::grammar::TraitBinder;

impl<T> TraitBinder<T>
where
    T: Term,
{
    pub fn open(&self) -> (Vec<KindedVarIndex>, T) {
        self.explicit_binder.open()
    }
}

impl<T> Term for TraitBinder<T> where T: Term {}

impl<T> Downcast<TraitBinder<T>> for TraitBinder<T>
where
    T: Term,
{
    fn downcast(&self) -> Option<TraitBinder<T>> {
        Some(self.clone())
    }
}

impl<T> UpcastFrom<TraitBinder<T>> for TraitBinder<T>
where
    T: Term,
{
    fn upcast_from(term: TraitBinder<T>) -> Self {
        term
    }
}

impl<T> Fold for TraitBinder<T>
where
    T: Term,
{
    fn substitute(&self, substitution_fn: formality_types::fold::SubstitutionFn<'_>) -> Self {
        TraitBinder {
            explicit_binder: self.explicit_binder.substitute(substitution_fn),
        }
    }

    fn free_variables(&self) -> Vec<formality_types::grammar::Variable> {
        self.explicit_binder.free_variables()
    }
}

impl<T> Debug for TraitBinder<T>
where
    T: Term,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.explicit_binder)
    }
}

impl<T> Parse for TraitBinder<T>
where
    T: Term,
{
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(scope: &formality_types::parse::Scope, text: &'t str) -> Option<(Self, &'t str)> {
        // parse the explicit bindings written by the user
        let text = expect_char('<', text)?;
        let (mut bindings, text) = Binding::parse_comma(scope, text);
        let text = expect_char('>', text)?;

        // insert the `Self` binding at position 0
        let (kvi, bound_var) = fresh_bound_var(ParameterKind::Ty);
        bindings.insert(
            0,
            Binding {
                name: "Self".to_string(),
                kvi,
                bound_var,
            },
        );

        // parse the contents with those names in scope
        let scope1 = scope.with_bindings(bindings.iter().map(|b| (b.name.clone(), b.kvi.to())));
        let (data, text) = T::parse(&scope1, text)?;

        let kvis: Vec<KindedVarIndex> = bindings.iter().map(|b| b.kvi).collect();
        let explicit_binder = Binder::new(&kvis, data);

        Some((TraitBinder { explicit_binder }, text))
    }
}
