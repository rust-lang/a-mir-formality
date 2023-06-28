use std::fmt::Debug;

use formality_types::{
    cast::To,
    derive_links::{DowncastTo, UpcastFrom},
    fold::Fold,
    grammar::{fresh_bound_var, Binder, BoundVar, ParameterKind},
    parse::{expect_char, Binding, Parse, ParseResult},
    term::Term,
    visit::Visit,
};

use crate::grammar::TraitBinder;

impl<T> TraitBinder<T>
where
    T: Term,
{
    pub fn open(&self) -> (Vec<BoundVar>, T) {
        self.explicit_binder.open()
    }
}

impl<T> Term for TraitBinder<T> where T: Term {}

impl<T> DowncastTo<TraitBinder<T>> for TraitBinder<T>
where
    T: Term,
{
    fn downcast_to(&self) -> Option<TraitBinder<T>> {
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

impl<T> Visit for TraitBinder<T>
where
    T: Term,
{
    fn free_variables(&self) -> Vec<formality_types::grammar::Variable> {
        self.explicit_binder.free_variables()
    }

    fn size(&self) -> usize {
        self.explicit_binder.size()
    }

    fn assert_valid(&self) {
        self.explicit_binder.assert_valid()
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
    fn parse<'t>(scope: &formality_types::parse::Scope, text: &'t str) -> ParseResult<'t, Self> {
        // parse the explicit bindings written by the user
        let ((), text) = expect_char('<', text)?;
        let (mut bindings, text) = Binding::parse_comma(scope, text, '>')?;
        let ((), text) = expect_char('>', text)?;

        // insert the `Self` binding at position 0
        let bound_var = fresh_bound_var(ParameterKind::Ty);
        bindings.insert(
            0,
            Binding {
                name: "Self".to_string(),
                bound_var,
            },
        );

        // parse the contents with those names in scope
        let scope1 =
            scope.with_bindings(bindings.iter().map(|b| (b.name.clone(), b.bound_var.to())));
        let (data, text) = T::parse(&scope1, text)?;

        let bound_vars: Vec<BoundVar> = bindings.into_iter().map(|b| b.bound_var).collect();
        let explicit_binder = Binder::new(&bound_vars, data);

        Ok((TraitBinder { explicit_binder }, text))
    }
}
