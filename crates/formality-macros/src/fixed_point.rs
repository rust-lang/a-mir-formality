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
//     thread_local! { static _CACHE: ... = ... }
//     fixed_point(
//         &_CACHE,
//         (k1, k2),
//         |(k1, k2)| <E>,
//         |(k1, k2)| <body>,
//     )
// }

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
            if ident.to_string() == "default" {
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

pub(crate) fn fixed_point(_args: FixedPointArgs, item_fn: syn::ItemFn) -> syn::Result<syn::ItemFn> {
    let syn::ItemFn {
        attrs: _,
        vis: _,
        sig,
        block: _,
    } = &item_fn;

    validate_fixed_point_sig(sig)?;

    Ok(item_fn)
}

fn validate_fixed_point_sig(sig: &syn::Signature) -> syn::Result<()> {
    for arg in &sig.inputs {
        match arg {
            syn::FnArg::Receiver(r) => {
                return Err(syn::Error::new_spanned(
                    r,
                    "fixed-point methods not yet supported",
                ));
            }
            syn::FnArg::Typed(syn::PatType { pat, ty, .. }) => {
                validate_arg_pattern(pat)?;
                validate_arg_ty(ty)?;
            }
        }
    }
    Ok(())
}

fn validate_arg_pattern(pat: &syn::Pat) -> syn::Result<()> {
    match pat {
        syn::Pat::Ident(_) => (),
        _ => {
            return Err(syn::Error::new_spanned(
                pat,
                "argument patterns not accepted in fixed-point functions",
            ))
        }
    }
    Ok(())
}

fn validate_arg_ty(ty: &syn::Type) -> syn::Result<()> {
    match ty {
        syn::Type::Reference(r) => {
            if r.mutability.is_some() {
                return Err(syn::Error::new_spanned(
                    ty,
                    "`&mut` arguments not permitted in fixed-point functions",
                ));
            }

            if let Some(_) = &r.lifetime {
                return Err(syn::Error::new_spanned(
                    ty,
                    "named lifetimes not permitted in fixed-point functions",
                ));
            }

            validate_ty(&r.elem)?;
        }
        _ => {
            validate_ty(ty)?;
        }
    }
    Ok(())
}

fn validate_ty(ty: &syn::Type) -> syn::Result<()> {
    match ty {
        syn::Type::ImplTrait(_) => {
            return Err(syn::Error::new_spanned(
                ty,
                "impl Trait types not allowed in fixed-point functions",
            ));
        }

        syn::Type::Reference(_) => {
            return Err(syn::Error::new_spanned(
                ty,
                "reference types only allowed at the top-level in fixed-point functions",
            ));
        }

        _ => Ok(()),
    }
}
