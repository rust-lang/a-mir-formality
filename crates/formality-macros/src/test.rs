use proc_macro::TokenStream;
use quote::quote;

pub(crate) fn test(args: TokenStream, mut item_fn: syn::ItemFn) -> syn::Result<syn::ItemFn> {
    let original_fn_body = item_fn.block.clone();
    item_fn.block = syn::parse2(quote! {
        {
            formality_core::with_tracing_logs(move || {
                #original_fn_body
            })
        }
    })
    .unwrap();
    Ok(item_fn)
}
