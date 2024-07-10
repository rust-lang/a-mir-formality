use proc_macro2::TokenStream;

#[derive(Default, Debug)]
pub(crate) struct Customize {
    pub parse: bool,
    pub debug: bool,
    pub constructors: bool,
    pub fold: bool,
    pub visit: bool,
    pub fuzz: bool,
}

impl syn::parse::Parse for Customize {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut result = Customize::default();

        let token_stream: TokenStream = input.parse()?;
        let mut tokens = token_stream.into_iter();

        while let Some(token) = tokens.next() {
            macro_rules! match_customization_field_or_error {
                ($($ident:ident,)*) => {
                    match token {
                        $(
                            proc_macro2::TokenTree::Ident(ident) if ident == stringify!($ident) => {
                                if result.$ident {
                                    return Err(syn::Error::new(ident.span(), &format!("already customizing {}", stringify!($ident))));
                                }
                                result.$ident = true;
                            }
                        )*

                        _ => {
                            return Err(syn::Error::new(
                                token.span(),
                                "unexpected token in customization",
                            ));
                        }
                    }
                };
            }

            match_customization_field_or_error! {
                parse,
                debug,
                constructors,
                fold,
                visit,
                fuzz,
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
