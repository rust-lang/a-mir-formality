use proc_macro2::TokenStream;
use quote::quote;
use syn::DeriveInput;

use crate::{
    attrs::{self, remove_formality_attributes},
    cast::{downcast_impls, upcast_impls},
    constructors::constructor_methods,
    debug::derive_debug_with_spec,
    fold::derive_fold,
    fuzz::derive_fuzz,
    parse::derive_parse_with_spec,
    spec::FormalitySpec,
    visit::derive_visit,
};

pub fn term(spec: Option<FormalitySpec>, mut input: DeriveInput) -> syn::Result<TokenStream> {
    let customize = attrs::customize(&input.attrs)?;
    let fold_impl = if customize.fold {
        Default::default()
    } else {
        derive_fold(synstructure::Structure::new(&input))
    };
    let visit_impl = if customize.visit {
        Default::default()
    } else {
        derive_visit(synstructure::Structure::new(&input))
    };
    let parse_impl = if customize.parse {
        Default::default()
    } else {
        derive_parse_with_spec(synstructure::Structure::new(&input), spec.as_ref())?
    };
    let debug_impl = if customize.debug {
        Default::default()
    } else {
        derive_debug_with_spec(synstructure::Structure::new(&input), spec.as_ref())
    };
    let term_impl = derive_term(synstructure::Structure::new(&input));
    let downcast_impls = downcast_impls(synstructure::Structure::new(&input));
    let upcast_impls = upcast_impls(synstructure::Structure::new(&input));
    let constructors = if customize.constructors {
        Default::default()
    } else {
        constructor_methods(synstructure::Structure::new(&input))
    };
    let fuzz_impl = if customize.fuzz {
        Default::default()
    } else {
        derive_fuzz(synstructure::Structure::new(&input))
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
        #fuzz_impl
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
