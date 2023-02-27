extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::quote;

pub(crate) fn derive_fold(mut s: synstructure::Structure) -> TokenStream {
    s.underscore_const(true);
    s.bind_with(|_| synstructure::BindStyle::Move);

    let substitute_body = s.each_variant(|vi| {
        let bindings = vi.bindings();
        vi.construct(|_, index| {
            let bind = &bindings[index];
            quote! {
                Fold::substitute(#bind, substitution_fn)
            }
        })
    });

    // s.add_bounds(synstructure::AddBounds::None);
    s.gen_impl(quote! {
        use crate::derive_links::{Fold, SubstitutionFn, Parameter, ParameterKind};

        gen impl Fold for @Self {
            fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
                match self {
                    #substitute_body
                }
            }
        }
    })
}
