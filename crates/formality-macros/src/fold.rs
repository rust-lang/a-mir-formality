extern crate proc_macro;

use proc_macro2::{Span, TokenStream};
use quote::quote;
use quote::ToTokens;
use syn::{parse_quote, DeriveInput, Ident, TypeParam, TypeParamBound};

use synstructure::decl_derive;

fn derive_fold(mut s: synstructure::Structure) -> TokenStream {
    s.underscore_const(true);
    s.bind_with(|_| synstructure::BindStyle::Ref);

    let substitute_body = s.each_variant(|vi| {
        let bindings = vi.bindings();
        vi.construct(|_, index| {
            let bind = &bindings[index];
            quote! {
                ::formality_types::fold::Fold::substitute(#bind, substitution_fn)
            }
        })
    });

    let free_variables_body = s
        .each(|field| quote!(output.extend(::formality_types::fold::Fold::free_variables(#field))));

    let shift_in_body = s.each_variant(|vi| {
        let bindings = vi.bindings();
        vi.construct(|_, index| {
            let bind = &bindings[index];
            quote! {
                ::formality_types::fold::Fold::shift_in(#bind)
            }
        })
    });

    s.add_bounds(synstructure::AddBounds::None);
    s.gen_impl(quote! {
        gen impl ::formality_types::fold::Fold for @Self {
            fn substitute(&self, substitution_fn: &mut impl FnMut(Variable) -> Parameter) -> Self {
                #substitute_body
            }

            fn free_variables(&self, substitution: &Substitution) -> Vec<Variable> {
                #free_variables_body
            }

            fn shift_in(&self, binders: usize) -> Self {
                #shift_in_body
            }
        }
    })
}
