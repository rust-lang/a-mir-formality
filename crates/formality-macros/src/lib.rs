use proc_macro::{Span, TokenStream};
use quote::quote;
use spec::FormalitySpec;
// use syn::DeriveInput;

extern crate proc_macro;

mod as_methods;
mod attrs;
mod cast;
mod constructors;
mod custom;
mod debug;
mod fixed_point;
mod fold;
mod fuzz;
mod parse;
mod precedence;
mod spec;
mod term;
mod test;
mod variable;
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

synstructure::decl_derive!([Fuzz] => fuzz::derive_fuzz);

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

/// Invoked like `respan!(X (Y...))` -- produces `Y...` with span from `X`.
#[proc_macro]
pub fn respan(t: TokenStream) -> TokenStream {
    let mut t = t.into_iter();
    let dummy = t.next().unwrap();
    let stream = t.next().unwrap();
    return match stream {
        proc_macro::TokenTree::Group(g) => set_span(pick_span(dummy), g.stream()),
        _ => unreachable!(),
    };

    fn pick_span(dummy: proc_macro::TokenTree) -> Span {
        match dummy {
            // Careful! The compiler creates groups for fragments
            // (e.g., $x:expr) that have the span of the macro that parsed them.
            // We want the span of the fragment's contents in that case.
            proc_macro::TokenTree::Group(g)
                if g.delimiter() == proc_macro::Delimiter::None && !g.stream().is_empty() =>
            {
                g.stream().into_iter().next().unwrap().span()
            }
            _ => dummy.span(),
        }
    }

    fn set_span(span: Span, tokens: TokenStream) -> TokenStream {
        tokens
            .into_iter()
            .map(|token| match token {
                proc_macro::TokenTree::Group(group) => {
                    let mut fixed =
                        proc_macro::Group::new(group.delimiter(), set_span(span, group.stream()));
                    fixed.set_span(span);
                    proc_macro::TokenTree::Group(fixed)
                }
                mut token => {
                    token.set_span(span);
                    token
                }
            })
            .collect()
    }
}
