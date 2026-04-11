use proc_macro::TokenStream;
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
mod parse;
mod precedence;
mod reject;
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
pub fn respan(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    respan_impl(stream.into()).into()
}

fn respan_impl(mut stream: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let (dummy, stream) = loop {
        let mut t = stream.into_iter();
        let dummy = t.next().unwrap();
        match dummy {
            proc_macro2::TokenTree::Group(ref g) => {
                if let Some(rest) = t.next() {
                    break (dummy, rest);
                }
                stream = g.stream();
            }
            _ => break (dummy, t.next().expect("rest")),
        }
    };
    return match stream {
        proc_macro2::TokenTree::Group(g) => set_span(pick_span(dummy), g.stream()),
        _ => unreachable!(),
    };

    fn pick_span(dummy: proc_macro2::TokenTree) -> proc_macro2::Span {
        match dummy {
            // Careful! The compiler creates groups for fragments
            // (e.g., $x:expr) that have the span of the macro that parsed them.
            // We want the span of the fragment's contents in that case.
            proc_macro2::TokenTree::Group(g)
                if g.delimiter() == proc_macro2::Delimiter::None && !g.stream().is_empty() =>
            {
                g.stream().into_iter().next().unwrap().span()
            }
            _ => dummy.span(),
        }
    }

    fn set_span(
        span: proc_macro2::Span,
        tokens: proc_macro2::TokenStream,
    ) -> proc_macro2::TokenStream {
        tokens
            .into_iter()
            .map(|token| match token {
                proc_macro2::TokenTree::Group(group) => {
                    let mut fixed =
                        proc_macro2::Group::new(group.delimiter(), set_span(span, group.stream()));
                    fixed.set_span(span);
                    proc_macro2::TokenTree::Group(fixed)
                }
                mut token => {
                    token.set_span(span);
                    token
                }
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::respan_impl;
    use quote::quote;

    #[test]
    fn test_respan_unpacking() {
        let input = quote! { X (a + b) };
        let output = respan_impl(input);
        assert_eq!(output.to_string(), "a + b");

        let input_wrapped = quote! { (X (a + b)) };
        let output_wrapped = respan_impl(input_wrapped);
        assert_eq!(output_wrapped.to_string(), "a + b");
    }

    #[test]
    fn test_respan_span_manipulation() {
        let input = quote! { X (a) };
        let mut t = input.clone().into_iter();
        let dummy = t.next().unwrap();
        let dummy_span = dummy.span();

        let output = respan_impl(input);
        let mut out_t = output.into_iter();
        let a = out_t.next().unwrap();
        let a_span = a.span();

        // Verify that the span of `a` has been updated to match `X`.
        assert_eq!(format!("{:?}", a_span), format!("{:?}", dummy_span));
    }
}
