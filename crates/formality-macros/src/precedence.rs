use std::str::FromStr;

use proc_macro2::{Ident, Literal, Span, TokenStream};
use quote::quote;
use syn::spanned::Spanned;

#[derive(Debug)]
pub(crate) enum Precedence {
    Defaulted,
    Parsed {
        level: usize,

        /// Will be either the name of a construct method on the
        /// `Parser::Associativity` type: `left``, `right``, or `none`.
        associativity: Ident,
    },
}

impl Precedence {
    pub fn expr(&self) -> TokenStream {
        match self {
            Precedence::Defaulted => quote!(formality_core::parse::Precedence::default()),
            Precedence::Parsed {
                level,
                associativity,
            } => {
                let level = Literal::usize_unsuffixed(*level);
                quote!(formality_core::parse::Precedence::#associativity(#level))
            }
        }
    }
}

impl Default for Precedence {
    fn default() -> Self {
        Precedence::Defaulted
    }
}

impl syn::parse::Parse for Precedence {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let token_stream: TokenStream = input.parse()?;
        let span = token_stream.span();
        let mut tokens = token_stream.into_iter().peekable();

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

        const VALID_ASSOCIATIVITIES: &[&str] = &["left", "right", "none"];
        let associativity = if let Some(comma_token) = tokens.next() {
            match &comma_token {
                proc_macro2::TokenTree::Punct(punct) if punct.as_char() == ',' => {
                    match tokens.next() {
                        Some(proc_macro2::TokenTree::Ident(ident))
                            if VALID_ASSOCIATIVITIES
                                .iter()
                                .any(|a| *a == ident.to_string()) =>
                        {
                            ident
                        }

                        _ => {
                            return Err(syn::Error::new(
                                comma_token.span(),
                                &format!(
                                    "expected valid associativity after comma, one of `{:?}`",
                                    VALID_ASSOCIATIVITIES
                                ),
                            ));
                        }
                    }
                }

                _ => {
                    return Err(syn::Error::new(
                        comma_token.span(),
                        "extra `,` followed by associativity",
                    ));
                }
            }
        } else {
            Ident::new("left", Span::call_site())
        };

        if let Some(token) = tokens.next() {
            return Err(syn::Error::new(token.span(), "extra tokens"));
        }

        Ok(Precedence::Parsed {
            level,
            associativity,
        })
    }
}
