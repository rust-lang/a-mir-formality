use proc_macro2::TokenStream;

#[derive(Default, Debug)]
pub(crate) struct Customize {
    pub parse: bool,
    pub debug: bool,
}

impl syn::parse::Parse for Customize {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut result = Customize::default();

        let token_stream: TokenStream = input.parse()?;
        let mut tokens = token_stream.into_iter();
        while let Some(token) = tokens.next() {
            match token {
                proc_macro2::TokenTree::Ident(ident) if ident == "parse" => {
                    if result.parse {
                        return Err(syn::Error::new(ident.span(), "already customizing parse"));
                    }
                    result.parse = true;
                }

                proc_macro2::TokenTree::Ident(ident) if ident == "debug" => {
                    if result.debug {
                        return Err(syn::Error::new(ident.span(), "already customizing debug"));
                    }
                    result.debug = true;
                }

                _ => {
                    return Err(syn::Error::new(
                        token.span(),
                        "unexpected token in customization",
                    ));
                }
            }

            if let Some(token) = tokens.next() {
                match token {
                    proc_macro2::TokenTree::Punct(p) if p.as_char() == ',' => (),
                    _ => {
                        return Err(syn::Error::new(
                            token.span(),
                            "unexpected token in customization",
                        ));
                    }
                }
            } else {
                break;
            }
        }

        Ok(result)
    }
}
