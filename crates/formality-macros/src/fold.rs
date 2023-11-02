extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::quote;

use crate::term::has_variable_attr;

pub(crate) fn derive_fold(mut s: synstructure::Structure) -> TokenStream {
    s.underscore_const(true);
    s.bind_with(|_| synstructure::BindStyle::Move);

    let substitute_body = s.each_variant(|vi| {
        let bindings = vi.bindings();
        if has_variable_attr(&vi.ast().attrs) {
            if bindings.len() != 1 {
                return syn::Error::new(
                    vi.ast().ident.span(),
                    "#[variable] can only be used on variants with 1 binding",
                )
                .into_compile_error();
            }

            let bi = &bindings[0];

            quote! {
                if let Some(p) = substitution_fn(*#bi) {
                    formality_core::Downcast::downcast(&p)
                        .unwrap_or_else(|| panic!("ill-kinded value `{:?}`", p))
                } else {
                    Clone::clone(self)
                }
            }
        } else {
            vi.construct(|_, index| {
                let bind = &bindings[index];
                quote! {
                    CoreFold::substitute(#bind, substitution_fn)
                }
            })
        }
    });

    // s.add_bounds(synstructure::AddBounds::None);
    s.gen_impl(quote! {
        use formality_core::{fold::CoreFold, fold::SubstitutionFn};

        gen impl CoreFold<crate::FormalityLang> for @Self {
            fn substitute(&self, substitution_fn: SubstitutionFn<'_, crate::FormalityLang>) -> Self {
                match self {
                    #substitute_body
                }
            }
        }
    })
}
