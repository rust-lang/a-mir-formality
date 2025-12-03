// expected input is something like
//
// #[fixed_point(default = <E>)]
// fn foo(k1: Key1, k2: &Key2) -> R
//
// where:
//
// * parameters may or may not be `&` type; they must be cloneable + ord
//   * parameters cannot have complex patterns to make my life easier
// * the return value R must be cloneable + ord
// * the default for the expression `<E>` is `Default::default`, and `E` may reference fn args
//
// generates:
//
// fn foo(k1: Key1, k2: &Key2) -> R {
//     thread_local! { static _CACHE: RefCell<FixedPointStack<(Key1, Key2), R>> = Default::default() }
//     fixed_point(
//         |(k1, k2)| {
//             tracing::debug_span!(
//                 stringify!(foo),
//                 ?k1,
//                 ?k2,
//             )
//         },
//         &_CACHE,
//         (k1, k2.clone()),
//         |(k1, k2)| <E>,
//         |(k1, ref k2)| <body>,
//     )
// }

use quote::quote;

#[derive(Default)]
pub(crate) struct FixedPointArgs {
    default: Option<syn::Expr>,
}

impl syn::parse::Parse for FixedPointArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut args: FixedPointArgs = Default::default();
        while !input.is_empty() {
            let ident: syn::Ident = input.parse()?;
            let _: syn::Token!(=) = input.parse()?;
            if ident == "default" {
                let expr: syn::Expr = input.parse()?;
                args.default = Some(expr);
            } else {
                return Err(syn::Error::new_spanned(
                    ident,
                    "valid arguments: ['default']",
                ));
            }

            // Parse comma after each argument.
            let comma: syn::Result<syn::Token!(,)> = input.parse();
            if comma.is_err() && !input.is_empty() {
                // If no comma, must be end of input.
                return Err(syn::Error::new_spanned(ident, "expected `,`"));
            }
        }
        Ok(args)
    }
}

struct Input {
    is_ref: bool,
    is_mut: Option<syn::Token!(mut)>,
    ident: syn::Ident,
    ty: syn::Type,
}

pub(crate) fn fixed_point(
    args: FixedPointArgs,
    mut item_fn: syn::ItemFn,
) -> syn::Result<syn::ItemFn> {
    let syn::ItemFn {
        attrs: _,
        vis: _,
        sig,
        block,
    } = &item_fn;

    let (inputs, output_ty) = validate_fixed_point_sig(sig)?;

    let input_tys: Vec<_> = inputs.iter().map(|i| &i.ty).collect();

    let thread_local = quote! {
        thread_local! {
            static __CACHE:
                std::cell::RefCell<
                    formality_core::fixed_point::FixedPointStack<
                        (#(#input_tys),*),
                        #output_ty
                    >
                >
            = Default::default()
        }
    };

    // names like k1, k2
    let input_names: Vec<_> = inputs.iter().map(|input| &input.ident).collect();

    // |(k1, k2)| tracing::debug_span(...)
    let tracing_span_expr = {
        let fn_name = &item_fn.sig.ident;
        quote!(
            |(#(#input_names),*)| {
                tracing::debug_span!(
                    stringify!(#fn_name),
                    #(?#input_names),*
                )
            }
        )
    };

    //         (k1, k2.clone()),
    let input_expr = {
        let input_exprs: Vec<_> = inputs
            .iter()
            .map(|Input { is_ref, ident, .. }| {
                if *is_ref {
                    quote! {Clone::clone(#ident)}
                } else {
                    quote! {#ident}
                }
            })
            .collect();
        quote!((#(#input_exprs,)*))
    };

    //         |(k1, k2)| <E>,
    let default_expr = {
        let default_pattern = quote!((#(#input_names),*));
        let default_body = args
            .default
            .map(|e| quote!(#e))
            .unwrap_or(quote!(Default::default()));
        quote!(
            #[allow(unused_variables)]
            |#default_pattern| #default_body
        )
    };

    //         |(k1, ref k2)| <body>,
    let body_expr = {
        let input_patterns: Vec<_> = inputs
            .iter()
            .map(
                |Input {
                     is_ref,
                     ident,
                     is_mut,
                     ty: _,
                 }| {
                    if *is_ref {
                        assert!(is_mut.is_none());
                        quote! {ref #ident}
                    } else {
                        quote! {#is_mut #ident}
                    }
                },
            )
            .collect();
        quote!(|(#(#input_patterns),*)| #block)
    };

    clear_mut_args(&mut item_fn.sig);
    item_fn.block = syn::parse(
        quote! {
            {
                #thread_local
                formality_core::fixed_point::fixed_point(
                    #tracing_span_expr,
                    &__CACHE,
                    #input_expr,
                    #default_expr,
                    #body_expr,
                )
            }
        }
        .into(),
    )
    .unwrap();

    Ok(item_fn)
}

fn clear_mut_args(sig: &mut syn::Signature) {
    for input in &mut sig.inputs {
        match input {
            syn::FnArg::Receiver(_) => {}
            syn::FnArg::Typed(t) => match &mut *t.pat {
                syn::Pat::Ident(i) => i.mutability = None,
                _ => panic!("unexpected pattern"),
            },
        }
    }
}

fn validate_fixed_point_sig(sig: &syn::Signature) -> syn::Result<(Vec<Input>, syn::Type)> {
    let mut inputs = vec![];
    for input in &sig.inputs {
        match input {
            syn::FnArg::Receiver(r) => {
                return Err(syn::Error::new_spanned(
                    r,
                    "fixed-point methods not yet supported",
                ));
            }
            syn::FnArg::Typed(syn::PatType { pat, ty, .. }) => {
                let (ident, is_mut) = validate_arg_pattern(pat)?;
                let (is_ref, ty) = validate_arg_ty(ty)?;

                if is_mut.is_some() && is_ref {
                    return Err(syn::Error::new_spanned(
                        input,
                        "variables can be mut or by-ref, but not both",
                    ));
                }

                inputs.push(Input {
                    is_ref,
                    is_mut,
                    ty,
                    ident,
                });
            }
        }
    }

    let output_ty = match &sig.output {
        syn::ReturnType::Default => {
            return Err(syn::Error::new_spanned(sig, "return type required"));
        }
        syn::ReturnType::Type(_, ty) => {
            validate_ty(ty)?;
            syn::Type::clone(ty)
        }
    };

    Ok((inputs, output_ty))
}

fn validate_arg_pattern(pat: &syn::Pat) -> syn::Result<(syn::Ident, Option<syn::Token!(mut)>)> {
    match pat {
        syn::Pat::Ident(ident) => {
            if let Some(r) = ident.by_ref {
                return Err(syn::Error::new_spanned(
                    r,
                    "ref patterns not accepted in fixed-point functions",
                ));
            }

            Ok((ident.ident.clone(), ident.mutability))
        }
        _ => Err(syn::Error::new_spanned(
            pat,
            "argument patterns not accepted in fixed-point functions",
        )),
    }
}

fn validate_arg_ty(ty: &syn::Type) -> syn::Result<(bool, syn::Type)> {
    match ty {
        syn::Type::Reference(r) => {
            if r.mutability.is_some() {
                return Err(syn::Error::new_spanned(
                    ty,
                    "`&mut` arguments not permitted in fixed-point functions",
                ));
            }

            if r.lifetime.is_some() {
                return Err(syn::Error::new_spanned(
                    ty,
                    "named lifetimes not permitted in fixed-point functions",
                ));
            }

            validate_ty(&r.elem)?;

            Ok((true, Clone::clone(&r.elem)))
        }
        _ => {
            validate_ty(ty)?;
            Ok((false, ty.clone()))
        }
    }
}

fn validate_ty(ty: &syn::Type) -> syn::Result<()> {
    match ty {
        syn::Type::ImplTrait(_) => Err(syn::Error::new_spanned(
            ty,
            "impl Trait types not allowed in fixed-point functions",
        )),

        syn::Type::Reference(_) => Err(syn::Error::new_spanned(
            ty,
            "reference types only allowed at the top-level in fixed-point functions",
        )),

        _ => Ok(()),
    }
}
