use proc_macro::TokenStream;

pub(crate) fn test(_args: TokenStream, mut item_fn: syn::ItemFn) -> syn::Result<syn::ItemFn> {
    let original_fn_body = item_fn.block.clone();
    item_fn.block = syn::parse_quote!({
        formality_core::with_tracing_logs(move || {
            #original_fn_body
        })
    });
    Ok(item_fn)
}
