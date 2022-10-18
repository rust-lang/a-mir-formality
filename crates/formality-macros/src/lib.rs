use proc_macro::TokenStream;
use spec::FormalitySpec;
use syn::DeriveInput;

extern crate proc_macro;

mod fold;
mod parse;
mod spec;
mod term;

synstructure::decl_derive!([Fold] => fold::derive_fold);
synstructure::decl_derive!([Parse, attributes(grammar)] => parse::derive_parse);

#[proc_macro_attribute]
pub fn term(args: TokenStream, input: TokenStream) -> TokenStream {
    let spec = syn::parse_macro_input!(args as FormalitySpec);
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    match term::term(spec, input) {
        Ok(s) => s.into(),
        Err(e) => e.into_compile_error().into(),
    }
}
