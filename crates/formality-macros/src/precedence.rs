use std::str::FromStr;

use proc_macro2::{Literal, TokenStream};
use syn::spanned::Spanned;

#[derive(Default, Debug)]
pub(crate) struct Precedence {
    pub level: usize,
}

impl Precedence {
    pub fn literal(&self) -> Literal {
        Literal::usize_unsuffixed(self.level)
    }
}

impl syn::parse::Parse for Precedence {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let token_stream: TokenStream = input.parse()?;
        let span = token_stream.span();
        let mut tokens = token_stream.into_iter();

        let Some(token) = tokens.next() else {
            return Err(syn::Error::new(span, "precedence expected"));
        };

        let level;
        match token {
            proc_macro2::TokenTree::Literal(l) => {
                let l_str = l.to_string();
                match usize::from_str(&l_str) {
                    Ok(l) => level = l,
                    Err(_) => return Err(syn::Error::new(l.span(), "integer precedence expected")),
                }
            }

            _ => {
                return Err(syn::Error::new(
                    token.span(),
                    "unexpected token in precedence",
                ));
            }
        }

        if let Some(token) = tokens.next() {
            return Err(syn::Error::new(token.span(), "extra tokens"));
        }

        Ok(Precedence { level })
    }
}
