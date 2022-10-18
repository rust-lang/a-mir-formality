use proc_macro2::TokenStream;
use quote::quote;
use syn::DeriveInput;

use crate::{
    fold::derive_fold,
    parse::{derive_parse, derive_parse_with_spec},
    spec::{self, FormalitySpec},
};

pub fn term(spec: Option<FormalitySpec>, mut input: DeriveInput) -> syn::Result<TokenStream> {
    let fold_impl = derive_fold(synstructure::Structure::new(&input));
    let parse_impl = derive_parse_with_spec(synstructure::Structure::new(&input), spec.as_ref());

    remove_grammar_attributes(&mut input);

    Ok(quote! {
        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
        #input

        #fold_impl
        #parse_impl
    })
}

fn remove_grammar_attributes(input: &mut DeriveInput) {
    if let syn::Data::Enum(v) = &mut input.data {
        for variant in &mut v.variants {
            variant.attrs.retain(|attr| !attr.path.is_ident("grammar"));
        }
    }
}
