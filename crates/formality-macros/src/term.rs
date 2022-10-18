use proc_macro2::TokenStream;
use quote::quote;
use syn::DeriveInput;

use crate::{
    fold::derive_fold,
    parse::derive_parse,
    spec::{self, FormalitySpec},
};

pub fn term(spec: FormalitySpec, input: DeriveInput) -> syn::Result<TokenStream> {
    let fold_impl = derive_fold(synstructure::Structure::new(&input));
    let parse_impl = derive_parse(synstructure::Structure::new(&input));
    Ok(quote! {
        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
        #input

        #fold_impl
        #parse_impl
    })
}
