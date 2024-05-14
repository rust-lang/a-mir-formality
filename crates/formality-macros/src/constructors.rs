use convert_case::{Case, Casing};
use proc_macro2::{Ident, TokenStream};
use quote::quote_spanned;
use synstructure::VariantInfo;

const RUST_KEYWORDS: &[&str] = &[
    "mut", "true", "false", "const", "static", "ref", "struct", "enum", "trait", "union", "fn",
    "use", "return", "move", "let", "break", "loop", "continue", "await", "if", "for", "unsafe",
];

/// Create methods to build this type.
///
/// For a struct, we create a single `new` method that takes each field.
///
/// For an enum, we create methods named after each variant.
pub(crate) fn constructor_methods(s: synstructure::Structure) -> TokenStream {
    match s.ast().data {
        syn::Data::Struct(_) => derive_new_for_struct(s),
        syn::Data::Enum(_) => derive_new_for_variants(s),
        syn::Data::Union(_) => Default::default(),
    }
}

fn derive_new_for_struct(s: synstructure::Structure<'_>) -> TokenStream {
    derive_new_for_variant(
        &s,
        &s.variants()[0],
        &Ident::new("new", s.ast().ident.span()),
    )
}

fn derive_new_for_variants(s: synstructure::Structure<'_>) -> TokenStream {
    s.variants()
        .iter()
        .map(|v| {
            let mut fn_name = v.ast().ident.to_string().to_case(Case::Snake);
            if RUST_KEYWORDS.iter().any(|&kw| kw == fn_name) {
                fn_name.push('_');
            }
            let fn_name = Ident::new(&fn_name, v.ast().ident.span());
            derive_new_for_variant(&s, v, &fn_name)
        })
        .collect()
}

fn derive_new_for_variant(
    s: &synstructure::Structure<'_>,
    v: &VariantInfo<'_>,
    fn_name: &Ident,
) -> TokenStream {
    let type_name = &s.ast().ident;
    let (impl_generics, type_generics, where_clauses) = s.ast().generics.split_for_impl();

    // If there are no bindings, not worth it.
    if v.bindings().is_empty() {
        return TokenStream::default();
    }

    let binding_names = v.bindings().iter().map(|b| &b.binding).collect::<Vec<_>>();
    let binding_types = v.bindings().iter().map(|b| &b.ast().ty).collect::<Vec<_>>();
    let construct = v.construct(|_b, i| {
        let name = binding_names[i];
        quote_spanned!(
            binding_names[i].span() =>
            formality_core::Upcast::upcast(#name)
        )
    });

    quote_spanned! { v.ast().ident.span() =>
        #[allow(dead_code)]
        impl #impl_generics #type_name #type_generics
        where #where_clauses
        {
            pub fn #fn_name(
                #(
                    #binding_names: impl formality_core::Upcast<#binding_types>,
                )*
            ) -> Self {
                #construct
            }
        }
    }
}
