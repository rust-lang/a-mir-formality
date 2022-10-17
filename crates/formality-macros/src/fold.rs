extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::quote;

pub(crate) fn derive_fold(mut s: synstructure::Structure) -> TokenStream {
    s.underscore_const(true);
    s.bind_with(|_| synstructure::BindStyle::Move);

    let substitute_body = s.each_variant(|vi| {
        let bindings = vi.bindings();
        vi.construct(|_, index| {
            let bind = &bindings[index];
            quote! {
                Fold::substitute(#bind, substitution_fn)
            }
        })
    });

    let free_variables_body = s.each(|field| quote!(output.extend(Fold::free_variables(#field))));

    // s.add_bounds(synstructure::AddBounds::None);
    s.gen_impl(quote! {
        use crate::derive_links::{Fold, SubstitutionFn, Variable, Parameter, ParameterKind};

        gen impl Fold for @Self {
            fn substitute(&self, substitution_fn: SubstitutionFn<'_>) -> Self {
                match self {
                    #substitute_body
                }
            }

            fn free_variables(&self) -> Vec<Variable> {
                let mut output = vec![];
                match self {
                    #free_variables_body
                }
                output
            }
        }
    })
}

#[test]
fn test_me() {
    synstructure::test_derive! {
        derive_fold {
            enum A<T> {
                B(i32, T),
                C(i32),
            }
        }
        expands to {
            const _ : (
            )
        = {
            use crate :: derive_links :: {
                Fold , SubstitutionFn , Variable , Parameter , ParameterKind }
            ;
            impl < T > Fold for A < T > where T : Fold {
                fn substitute (
                    & self , substitution_fn : SubstitutionFn < '_ >)
                -> Self {
                    match self {
                        A :: B (
                            __binding_0 , __binding_1 ,)
                        => {
                            A :: B (
                                Fold :: substitute (
                                    __binding_0 , substitution_fn)
                                , Fold :: substitute (
                                    __binding_1 , substitution_fn)
                                ,)
                            }
                        A :: C (
                            __binding_0 ,)
                        => {
                            A :: C (
                                Fold :: substitute (
                                    __binding_0 , substitution_fn)
                                ,)
                            }
                        }
                    }
                fn free_variables (
                    & self)
                -> Vec < Variable > {
                    let mut output = vec ! [
                        ]
                    ;
                    match self {
                        A :: B (
                            __binding_0 , __binding_1 ,)
                        => {
                            {
                                output . extend (
                                    Fold :: free_variables (
                                        __binding_0)
                                    )
                                }
                            {
                                output . extend (
                                    Fold :: free_variables (
                                        __binding_1)
                                    )
                                }
                            }
                        A :: C (
                            __binding_0 ,)
                        => {
                            {
                                output . extend (
                                    Fold :: free_variables (
                                        __binding_0)
                                    )
                                }
                            }
                        }
                    output }
                }
            }
        ;
        }
        no_build
    }
}
