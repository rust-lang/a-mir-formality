use convert_case::{Case, Casing};
use proc_macro2::{Ident, TokenStream};
use quote::{quote, quote_spanned};
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

    let mut ts = s.gen_impl(quote! {
        use formality_core::{DowncastTo};

        gen impl DowncastTo<(#(#binding_tys),*)> for @Self {
            fn downcast_to(&self) -> Option<(#(#binding_tys),*)> {
                match self {
                    #downcast_fn
                }
            }
        }
    });

    if binding_tys.len() == 1 && matches!(s.ast().data, syn::Data::Enum(_)) {
        let binding_ty = &binding_tys[0];
        let type_name = &s.ast().ident;
        let variant_name = &v.ast().ident;
        let as_method_name = Ident::new(
            &format!("as_{}", variant_name.to_string().to_case(Case::Snake)),
            v.ast().ident.span(),
        );
        let (impl_generics, type_generics, where_clauses) = s.ast().generics.split_for_impl();

        let match_arms = s.each_variant(|variant_info| {
            if variant_info.ast().ident == v.ast().ident {
                let bindings = variant_info.bindings();
                let binding_idents: Vec<&syn::Ident> =
                    bindings.iter().map(|b| &b.binding).collect();
                quote! { Some((#(#binding_idents),*)) }
            } else {
                quote! { None }
            }
        });

        let as_impl = quote_spanned! { v.ast().ident.span() =>
            #[allow(dead_code)]
            impl #impl_generics #type_name #type_generics
            where #where_clauses
            {
                pub fn #as_method_name(&self) -> Option<& #binding_ty> {
                    match self {
                        #match_arms
                    }
                }
            }
        };

        ts.extend(as_impl);
    }

    ts
}
