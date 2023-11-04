use proc_macro2::TokenStream;
use quote::quote;
use syn::Type;
use synstructure::VariantInfo;

use crate::attrs::has_isa_attr;

pub(crate) fn upcast_impls(s: synstructure::Structure) -> Vec<TokenStream> {
    let num_variants = s.variants().len();
    s.variants()
        .iter()
        .filter(|v| num_variants == 1 || has_isa_attr(v.ast().attrs))
        .map(|v| upcast_to_variant(&s, v))
        .chain(Some(self_upcast(&s)))
        .collect()
}

fn self_upcast(s: &synstructure::Structure) -> TokenStream {
    s.gen_impl(quote! {
        use formality_core::UpcastFrom;

        gen impl UpcastFrom<Self> for @Self {
            fn upcast_from(term: Self) -> Self {
                term
            }
        }
    })
}

fn upcast_to_variant(s: &synstructure::Structure, v: &VariantInfo) -> TokenStream {
    let binding_tys: Vec<&Type> = v.bindings().iter().map(|b| &b.ast().ty).collect();
    let binding_names: Vec<&syn::Ident> = v.bindings().iter().map(|b| &b.binding).collect();
    let variant_construct = v.construct(|_field, index| &binding_names[index]);

    s.gen_impl(quote! {
        use formality_core::{UpcastFrom};

        gen impl UpcastFrom<(#(#binding_tys),*)> for @Self {
            fn upcast_from(term: (#(#binding_tys),*)) -> Self {
                let (#(#binding_names),*) = term;
                #variant_construct
            }
        }
    })
}

pub(crate) fn downcast_impls(s: synstructure::Structure) -> Vec<TokenStream> {
    let num_variants = s.variants().len();
    s.variants()
        .iter()
        .filter(|v| num_variants == 1 || has_isa_attr(v.ast().attrs))
        .map(|v| downcast_to_variant(&s, v))
        .chain(Some(self_downcast(&s)))
        .collect()
}

fn self_downcast(s: &synstructure::Structure) -> TokenStream {
    s.gen_impl(quote! {
        use formality_core::{DowncastTo};

        gen impl DowncastTo<Self> for @Self {
            fn downcast_to(&self) -> Option<Self> {
                Some(Self::clone(self))
            }
        }
    })
}

fn downcast_to_variant(s: &synstructure::Structure, v: &VariantInfo) -> TokenStream {
    let binding_tys: Vec<&Type> = v.bindings().iter().map(|b| &b.ast().ty).collect();

    let downcast_fn = s.each_variant(|variant_info| {
        if variant_info.ast().ident == v.ast().ident {
            let bindings = variant_info.bindings();
            let binding_idents: Vec<&syn::Ident> = bindings.iter().map(|b| &b.binding).collect();
            quote! { Some((#(Clone::clone(#binding_idents)),*)) }
        } else {
            quote! { None }
        }
    });

    s.gen_impl(quote! {
        use formality_core::{DowncastTo};

        gen impl DowncastTo<(#(#binding_tys),*)> for @Self {
            fn downcast_to(&self) -> Option<(#(#binding_tys),*)> {
                match self {
                    #downcast_fn
                }
            }
        }
    })
}
