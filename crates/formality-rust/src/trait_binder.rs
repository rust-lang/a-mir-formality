use std::fmt::Debug;

use formality_core::{
    binder::fuzz::KindVec,
    fold::CoreFold,
    fuzz::Fuzzable,
    parse::{Binding, CoreParse, ParseResult, Parser, Scope},
    term::CoreTerm,
    visit::CoreVisit,
    DowncastTo, UpcastFrom,
};
use formality_types::{
    grammar::{Binder, BoundVar, ParameterKind},
    rust::Term,
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

impl<T> Fuzzable<FormalityLang> for TraitBinder<T>
where
    T: CoreTerm<FormalityLang>,
{
    fn is_binder() -> bool {
        true
    }

    fn estimate_cardinality(cx: &mut formality_core::fuzz::FuzzCx<'_, FormalityLang>) -> f64 {
        cx.enter_estimate_cardinality::<Self>(|guard| {
(guard.estimate_cardinality::<ParameterKind>().powi(guard.binder_range().len() as i32) + 1) 
                * guard.estimate_cardinality::<T>() // and then we generate the T
        })
    }

    fn fuzz(cx: &mut formality_core::fuzz::FuzzCx<'_, FormalityLang>) -> Option<Self> {
        cx.enter_fuzz::<Self>(|guard| {
            let kinds: KindVec<FormalityLang> = guard.fuzz()?;
            let kinds: Vec<ParameterKind> = Some(ParameterKind::Ty)
                .into_iter()
                .chain(kinds.into_vec())
                .collect();
            let explicit_binder =
                guard.introduce_free_variables_of_kinds(&kinds, |guard, variables| {
                    let bound_term = guard.fuzz::<T>()?;
                    Some(Binder::new(variables, bound_term))
                })?;
            Some(TraitBinder { explicit_binder })
        })
    }
}

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

impl<T> CoreFold<FormalityLang> for TraitBinder<T>
where
    T: Term,
{
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
            let mut bindings = match p.expect_char('<') {
                Ok(()) => {
                    let bindings: Vec<Binding<FormalityLang>> = p.comma_nonterminal()?;
                    p.expect_char('>')?;
                    bindings
                }
                Err(_) => {
                    // If we don't see a `<`, assume there are no add'l bound variables.
                    vec![]
                }
            };

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
            let scope1 = scope.with_bindings(bindings.iter().map(|b| (&b.name, &b.bound_var)));
            let data: T = p.with_scope(scope1, |p| p.nonterminal())?;

            let bound_vars: Vec<BoundVar> = bindings.iter().map(|b| b.bound_var).collect();
            let explicit_binder = Binder::new(bound_vars, data);

            Ok(TraitBinder { explicit_binder })
        })
    }
}
