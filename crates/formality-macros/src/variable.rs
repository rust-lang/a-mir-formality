use proc_macro2::TokenStream;

/// The contents of a `#[variable]` attribute. Currently just a kind.
pub struct Variable {
    pub kind: syn::Expr,
}

impl syn::parse::Parse for Variable {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let kind: syn::Expr = input.parse()?;

        let tokens: TokenStream = input.parse()?;
        let mut tokens = tokens.into_iter();
        if let Some(token) = tokens.next() {
            return Err(syn::Error::new(token.span(), "extra tokens"));
        }

        Ok(Variable { kind })
    }
}
