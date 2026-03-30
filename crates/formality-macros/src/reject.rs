//! Parsing and codegen for `#[reject(PATTERN)]` attributes.
//!
//! A `#[reject]` attribute on a variant or struct silently drops parse results
//! matching the given pattern.
//!
//! All fields must be listed explicitly (using `_` for wildcards), or trailing
//! `..` can be used to skip remaining fields:
//!
//! ```rust,ignore
//! #[reject(Perm::Apply(..), _)]    // OK: both fields listed
//! #[reject(Perm::Apply(..), ..)]   // OK: first field checked, rest skipped
//! #[reject(Perm::Apply(..))]       // ERROR: missing field (use `..` or `_`)
//! ```

use proc_macro2::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};

/// A single parsed `#[reject(...)]` attribute.
///
/// Contains per-field patterns. Each is either a wildcard (`_`) meaning
/// "no constraint" or a token stream to use inside `matches!`.
#[derive(Debug)]
pub(crate) struct RejectAttr {
    pub fields: Vec<RejectFieldPattern>,
    /// True if the attribute ended with `..` (skip remaining fields).
    pub has_trailing_dotdot: bool,
}

/// A single field's pattern within a `#[reject(...)]` attribute.
#[derive(Debug)]
pub(crate) enum RejectFieldPattern {
    /// `_` — skip this field (no constraint).
    Wildcard,
    /// A named field pattern: `name: Pattern`.
    Named {
        name: syn::Ident,
        pattern: TokenStream,
    },
    /// A positional field pattern (token stream to use in `matches!`).
    Positional(TokenStream),
}

impl Parse for RejectAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut fields = Vec::new();
        let mut has_trailing_dotdot = false;

        while !input.is_empty() {
            // Check for trailing `..`
            if input.peek(syn::Token![..]) {
                input.parse::<syn::Token![..]>()?;
                has_trailing_dotdot = true;
                if !input.is_empty() {
                    return Err(input.error("`..` must be the last element in #[reject]"));
                }
                break;
            }

            // Check for `_`
            if input.peek(syn::Token![_]) {
                input.parse::<syn::Token![_]>()?;
                fields.push(RejectFieldPattern::Wildcard);
            } else if input.peek(syn::Ident) && input.peek2(syn::Token![:]) {
                // Named field: `name: pattern`
                // But be careful: `Perm::Apply(..)` also starts with an ident.
                // We distinguish by checking if the *second* token is `:` (not `::`).
                let fork = input.fork();
                let name: syn::Ident = fork.parse()?;
                // Check it's `:` not `::`
                if fork.peek(syn::Token![:]) && !fork.peek(syn::Token![::]) {
                    // It's a named field pattern
                    input.parse::<syn::Ident>()?; // consume the name
                    input.parse::<syn::Token![:]>()?; // consume the `:`

                    // Check if the pattern is `_`
                    if input.peek(syn::Token![_])
                        && (input.peek2(syn::Token![,])
                            || input.is_empty()
                            || input.peek2(syn::Token![..]))
                    {
                        input.parse::<syn::Token![_]>()?;
                        fields.push(RejectFieldPattern::Named {
                            name,
                            pattern: quote!(_),
                        });
                    } else {
                        // Parse the rest as a token stream until the next top-level comma or end
                        let pattern = parse_until_comma(input)?;
                        fields.push(RejectFieldPattern::Named { name, pattern });
                    }
                } else {
                    // It's a positional pattern starting with an ident (e.g., Perm::Apply(..))
                    let pattern = parse_until_comma(input)?;
                    fields.push(RejectFieldPattern::Positional(pattern));
                }
            } else {
                // Positional pattern
                let pattern = parse_until_comma(input)?;
                fields.push(RejectFieldPattern::Positional(pattern));
            }

            // Consume comma separator if present
            if input.peek(syn::Token![,]) {
                input.parse::<syn::Token![,]>()?;
            }
        }

        Ok(RejectAttr {
            fields,
            has_trailing_dotdot,
        })
    }
}

/// Parse tokens until the next top-level comma or end of input.
/// Respects nesting in `()`, `[]`, `{}`.
fn parse_until_comma(input: ParseStream) -> syn::Result<TokenStream> {
    let mut tokens = TokenStream::new();
    while !input.is_empty() && !input.peek(syn::Token![,]) && !input.peek(syn::Token![..]) {
        let tt: proc_macro2::TokenTree = input.parse()?;
        tokens.extend(std::iter::once(tt));
    }
    if tokens.is_empty() {
        return Err(input.error("expected a pattern"));
    }
    Ok(tokens)
}
