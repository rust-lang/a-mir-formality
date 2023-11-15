use proc_macro2::TokenStream;
use quote::quote;
use syn::DeriveInput;

use crate::{
    attrs::{self, remove_formality_attributes},
    cast::{downcast_impls, upcast_impls},
    constructors::constructor_methods,
    debug::derive_debug_with_spec,
    fold::derive_fold,
    parse::derive_parse_with_spec,
    spec::FormalitySpec,
    visit::derive_visit,
};

pub fn term(spec: Option<FormalitySpec>, mut input: DeriveInput) -> syn::Result<TokenStream> {
    let customize = attrs::customize(&input.attrs)?;
    let fold_impl = derive_fold(synstructure::Structure::new(&input));
    let visit_impl = derive_visit(synstructure::Structure::new(&input));
    let parse_impl = if customize.parse {
        None
    } else {
        Some(derive_parse_with_spec(
            synstructure::Structure::new(&input),
            spec.as_ref(),
        )?)
    };
    let debug_impl = if customize.debug {
        None
    } else {
        Some(derive_debug_with_spec(
            synstructure::Structure::new(&input),
            spec.as_ref(),
        ))
    };
    let term_impl = derive_term(synstructure::Structure::new(&input));
    let downcast_impls = downcast_impls(synstructure::Structure::new(&input));
    let upcast_impls = upcast_impls(synstructure::Structure::new(&input));
    let constructors = if customize.constructors {
        None
    } else {
        Some(constructor_methods(synstructure::Structure::new(&input)))
    };
    remove_formality_attributes(&mut input);

    Ok(quote! {
        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #input

        #fold_impl
        #visit_impl
        #parse_impl
        #debug_impl
        #term_impl
        #(#downcast_impls)*
        #(#upcast_impls)*
        #constructors
    })
}

fn derive_term(mut s: synstructure::Structure) -> TokenStream {
    s.underscore_const(true);
    s.bind_with(|_| synstructure::BindStyle::Move);

    // s.add_bounds(synstructure::AddBounds::None);
    s.gen_impl(quote! {
        use formality_core::term::CoreTerm;

        gen impl CoreTerm<crate::FormalityLang> for @Self {
        }
    })
}
