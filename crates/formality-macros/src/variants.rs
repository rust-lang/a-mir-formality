use proc_macro2::TokenStream;
use quote::quote;

/// For C-like enums, define a method `variants` that returns all the variants
pub(crate) fn variants_impls(s: synstructure::Structure) -> Vec<TokenStream> {
    let syn::Data::Enum(e) = &s.ast().data else { return vec![] };

    let is_c_like = e.variants.iter().all(|v| v.fields.is_empty());
    if !is_c_like {
        return vec![];
    }

    let ident = &s.ast().ident;
    let variant_names: Vec<_> = e.variants.iter().map(|v| &v.ident).collect();

    vec![quote! {
        impl #ident {
            pub fn variants() -> &'static [Self] {
                &[
                    #(
                        Self :: #variant_names,
                    )*
                ]
            }
        }
    }]
}
