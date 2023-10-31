extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::quote;

pub(crate) fn derive_visit(mut s: synstructure::Structure) -> TokenStream {
    s.underscore_const(true);
    s.bind_with(|_| synstructure::BindStyle::Move);

    let free_variables_body = s.each(
        |field| quote!(output.extend(<_ as CoreVisit<crate::FormalityLang>>::free_variables(#field))),
    );

    let size_body =
        s.each(|field| quote!(__sum += <_ as CoreVisit<crate::FormalityLang>>::size(#field)));

    let assert_valid_body =
        s.each(|field| quote!(<_ as CoreVisit<crate::FormalityLang>>::assert_valid(#field)));

    // s.add_bounds(synstructure::AddBounds::None);
    s.gen_impl(quote! {
        use formality_core::{visit::CoreVisit, variable::CoreVariable};

        gen impl CoreVisit<crate::FormalityLang> for @Self {
            fn free_variables(&self) -> Vec<CoreVariable<crate::FormalityLang>> {
                let mut output = vec![];
                match self {
                    #free_variables_body
                }
                output
            }

            fn size(&self) -> usize {
                let mut __sum = 0;
                __sum += 1;
                match self {
                    #size_body
                }
                __sum
            }

            fn assert_valid(&self) {
                match self {
                    #assert_valid_body
                }
            }
        }
    })
}
