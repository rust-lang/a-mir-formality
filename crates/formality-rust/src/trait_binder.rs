use std::fmt::Debug;

use crate::grammar::{Binder, BoundVar, ParameterKind};
use crate::rust::Term;
use formality_core::{
    fold::CoreFold,
    parse::{ActiveVariant, Binding, CoreParse, ParseResult, Parser, Scope},
    term::CoreTerm,
    visit::CoreVisit,
    DowncastTo, UpcastFrom,
};

use crate::{grammar::TraitBinder, FormalityLang};

impl<T> TraitBinder<T>
where
    T: Term,
{
    pub fn open(&self) -> (Vec<BoundVar>, T) {
        self.explicit_binder.open()
    }
}

impl<T> CoreTerm<FormalityLang> for TraitBinder<T> where T: Term {}

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

impl<T> CoreVisit<FormalityLang> for TraitBinder<T>
where
    T: Term,
{
    fn free_variables(&self) -> Vec<crate::grammar::Variable> {
        self.explicit_binder.free_variables()
    }

    fn size(&self) -> usize {
        self.explicit_binder.size()
    }

    fn assert_valid(&self) {
        self.explicit_binder.assert_valid()
    }
}

impl<T> CoreFold<FormalityLang> for TraitBinder<T>
where
    T: Term,
{
    type Output = Self;

    fn substitute(
        &self,
        substitution_fn: formality_core::fold::SubstitutionFn<'_, FormalityLang>,
    ) -> Self {
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
        // In Debug output, we include the `Self` to avoid confusion -- is this good?
        write!(f, "{:?}", self.explicit_binder)
    }
}

impl<T> CoreParse<FormalityLang> for TraitBinder<T>
where
    T: Term,
{
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(scope: &Scope<FormalityLang>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "TraitBinder", |p| {
            let parse_body =
                |mut bindings: Vec<Binding<FormalityLang>>,
                 p: &mut ActiveVariant<'_, 't, FormalityLang>| {
                    // insert the `Self` binding at position 0
                    let bound_var = BoundVar::fresh(ParameterKind::Ty);
                    bindings.insert(
                        0,
                        Binding {
                            name: "Self".to_string(),
                            bound_var,
                        },
                    );

                    // parse the contents with those names in scope
                    let scope1 =
                        scope.with_bindings(bindings.iter().map(|b| (&b.name, &b.bound_var)));
                    p.with_scope(scope1, |p| {
                        p.each_nonterminal(|data: T, p| {
                            let bound_vars: Vec<BoundVar> =
                                bindings.iter().map(|b| b.bound_var).collect();
                            let explicit_binder = Binder::new(bound_vars, data);
                            p.ok(TraitBinder { explicit_binder })
                        })
                    })
                };

            match p.expect_char('<') {
                Ok(()) => p.each_comma_nonterminal(|bindings: Vec<Binding<FormalityLang>>, p| {
                    p.expect_char('>')?;
                    parse_body(bindings, p)
                }),
                Err(_) => {
                    // If we don't see a `<`, assume there are no add'l bound variables.
                    parse_body(vec![], p)
                }
            }
        })
    }
}
