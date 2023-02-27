extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::quote;

pub(crate) fn derive_visit(mut s: synstructure::Structure) -> TokenStream {
    s.underscore_const(true);
    s.bind_with(|_| synstructure::BindStyle::Move);

    let free_variables_body = s.each(|field| quote!(output.extend(Visit::free_variables(#field))));

    // s.add_bounds(synstructure::AddBounds::None);
    s.gen_impl(quote! {
        use crate::derive_links::{Visit, Variable};

        gen impl Visit for @Self {
            fn free_variables(&self) -> Vec<Variable> {
                let mut output = vec![];
                match self {
                    #free_variables_body
                }
                output
            }
        }
    })
}
