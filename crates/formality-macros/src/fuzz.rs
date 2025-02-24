extern crate proc_macro;

use proc_macro2::{Span, TokenStream};
use quote::quote;

pub(crate) fn derive_fuzz(mut s: synstructure::Structure) -> TokenStream {
    s.underscore_const(true);
    s.bind_with(|_| synstructure::BindStyle::Move);

    let cx = syn::Ident::new("cx", Span::call_site());
    let guard = syn::Ident::new("guard", Span::call_site());

    // The cardinarlity of a variant is the product of its fields.
    let estimate_cardinality_variants: Vec<_> = s
        .variants()
        .iter()
        .map(|v| {
            let estimate_cardinality_fields: Vec<_> = v
                .ast()
                .fields
                .iter()
                .map(|f| &f.ty)
                .map(|ty| quote!(#guard.estimate_cardinality::<#ty>()))
                .collect();
            quote!(1.0 #(* #estimate_cardinality_fields)*)
        })
        .collect();

    // The cardinarily of a type is equal to the sum of the cardinalities of its variants.
    let estimate_cardinality_body = quote!(0.0 #( + #estimate_cardinality_variants)*);

    // Fuzzing a value involves first selecting a variant.
    let fuzz_constructors: Vec<_> = s
        .variants()
        .iter()
        .map(|v| {
            let fuzz_call = v.construct(|field, _| {
                let ty = &field.ty;
                quote!(#guard.fuzz::<#ty>()?)
            });

            quote!(&|#guard| Some(#fuzz_call))
        })
        .collect();

    let fuzz_body = quote!({
        let weights = &[#(#estimate_cardinality_variants),*];
        let constructors: &[
            &dyn std::ops::Fn(
                &mut formality_core::fuzz::EnterGuard<'_, '_, crate::FormalityLang, Self>,
            ) -> Option<Self>
        ] = &[#(#fuzz_constructors),*];
        #guard.fuzz_sum_type(
            weights,
            constructors,
        )
    });

    let tokens = s.gen_impl(quote! {
        use formality_core::fuzz::{FuzzCx, Fuzzable};

        gen impl Fuzzable<crate::FormalityLang> for @Self {
            fn estimate_cardinality(#cx: &mut FuzzCx<'_, crate::FormalityLang>) -> f64 {
                #cx.enter_estimate_cardinality::<Self>(|#guard| #estimate_cardinality_body)
            }

            fn fuzz(#cx: &mut FuzzCx<'_, crate::FormalityLang>) -> Option<Self> {
                #cx.enter_fuzz::<Self>(|#guard| #fuzz_body)
            }
        }
    });

    tokens
}
