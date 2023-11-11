use proc_macro::TokenStream;
use quote::quote;
use spec::FormalitySpec;
// use syn::DeriveInput;

extern crate proc_macro;

mod as_methods;
mod attrs;
mod cast;
mod custom;
mod debug;
mod fixed_point;
mod fold;
mod parse;
mod precedence;
mod spec;
mod term;
mod test;
mod visit;

// synstructure::decl_derive!([Fold] => fold::derive_fold);
// synstructure::decl_derive!([Parse, attributes(grammar)] => parse::derive_parse);

#[proc_macro_attribute]
pub fn term(args: TokenStream, input: TokenStream) -> TokenStream {
    let spec = if args.is_empty() {
        None
    } else {
        Some(syn::parse_macro_input!(args as FormalitySpec))
    };
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    match term::term(spec, input) {
        Ok(s) => s.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

synstructure::decl_derive!([Visit] => visit::derive_visit);

#[proc_macro_attribute]
pub fn fixed_point(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = syn::parse_macro_input!(args as fixed_point::FixedPointArgs);
    let input = syn::parse_macro_input!(input as syn::ItemFn);
    match fixed_point::fixed_point(args, input) {
        Ok(s) => quote!(#s).into(),
        Err(e) => e.into_compile_error().into(),
    }
}

#[proc_macro_attribute]
pub fn test(args: TokenStream, input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::ItemFn);
    match test::test(args, input) {
        Ok(s) => quote!(#[::core::prelude::v1::test] #s).into(),
        Err(e) => e.into_compile_error().into(),
    }
}
