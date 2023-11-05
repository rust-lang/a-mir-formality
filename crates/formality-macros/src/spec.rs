use std::iter::Peekable;

use proc_macro2::{Group, Ident, Punct, TokenStream, TokenTree};
use syn::spanned::Spanned;

/// The "formality spec" guides parsing and serialization.
///
/// It has a very simple format consisting of a series of tokens:
///
/// * an identifier like `foo` is parsed as a keyword
/// * a field like `$foo` parses the type of the declared field
///     * you can also do `$*foo` to use the `parse_many` option
/// * a character like `<` is parsed as is; a group like `[..]` parses a `[`, the contents, and then `]`
#[derive(Debug)]
pub struct FormalitySpec {
    pub symbols: Vec<FormalitySpecSymbol>,
}

#[derive(Debug)]
pub enum FormalitySpecSymbol {
    /// `$foo` or `$foo*` -- indicates we should parse the type of the given field.
    Field { name: Ident, mode: FieldMode },

    /// `foo` -- indicates we should parse the given keyword.
    Keyword { ident: Ident },

    /// `<` -- indicates we should parse the given char. We currently ignoring the spacing rules.
    Char { punct: Punct },

    /// Specific delimeter (e.g., `(`) we should parse.
    Delimeter { text: char },
}

#[derive(Debug)]
pub enum FieldMode {
    /// $x -- just parse `x`
    Single,

    /// $<x> -- `x` is a `Vec<E>`, parse `<E0,...,En>`
    /// $[x] -- `x` is a `Vec<E>`, parse `[E0,...,En]`
    /// $(x) -- `x` is a `Vec<E>`, parse `(E0,...,En)`
    /// $<?x> -- `x` is a `Vec<E>`, parse `<E0,...,En>` or empty list
    /// $[?x] -- `x` is a `Vec<E>`, parse `[E0,...,En]` or empty list
    /// $(?x) -- `x` is a `Vec<E>`, parse `(E0,...,En)` or empty list
    ///
    /// If the next op is a fixed character, stop parsing when we see that.
    /// Otherwise parse as many we can greedily.
    DelimitedVec {
        open: char,
        optional: bool,
        close: char,
    },

    /// $*x -- `x` is a `Vec<E>`, parse multiple `E`
    ///
    /// If the next op is a fixed character, stop parsing when we see that.
    /// Otherwise parse as many we can greedily.
    Many,

    /// $*x -- `x` is a `Vec<E>`, parse comma separated list of `E`
    /// (with optonal trailing comma)
    ///
    /// If the next op is a fixed character, stop parsing when we see that.
    /// Otherwise parse as many we can greedily.
    Comma,

    /// $?x -- parse `x` if we can, but otherwise use `Default`
    Optional,
}

impl syn::parse::Parse for FormalitySpec {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let token_stream: TokenStream = input.parse()?;
        let mut symbols = vec![];
        token_stream_to_symbols(&mut symbols, token_stream)?;
        Ok(FormalitySpec { symbols })
    }
}

fn token_stream_to_symbols(
    symbols: &mut Vec<FormalitySpecSymbol>,
    token_stream: TokenStream,
) -> syn::Result<()> {
    use FormalitySpecSymbol::*;

    let mut tokens = token_stream.into_iter();

    while let Some(token) = tokens.next() {
        match token {
            proc_macro2::TokenTree::Group(v) => {
                let open_close = open_close(&v);

                if let Some((ch, _)) = open_close {
                    symbols.push(Delimeter { text: ch });
                }
                token_stream_to_symbols(symbols, v.stream())?;
                if let Some((_, ch)) = open_close {
                    symbols.push(Delimeter { text: ch });
                }
            }
            proc_macro2::TokenTree::Ident(ident) => symbols.push(Keyword { ident }),
            proc_macro2::TokenTree::Punct(punct) => match punct.as_char() {
                '$' => symbols.push(parse_variable_binding(punct, &mut tokens)?),
                _ => symbols.push(Char { punct }),
            },
            proc_macro2::TokenTree::Literal(_) => {
                let message = "unexpected literal in parse string";
                return Err(syn::Error::new(token.span(), message));
            }
        }
    }
    Ok(())
}

/// Invoked after we have seen a `$`. We expect:
///
/// * `foo` -- an ident
/// * `*foo` -- a
///
/// or we could also see a `$`, in which case user wrote `$$`, and we treat that as a single
/// `$` sign.
fn parse_variable_binding(
    dollar_token: Punct,
    tokens: &mut impl Iterator<Item = TokenTree>,
) -> syn::Result<FormalitySpecSymbol> {
    let dollar_token = &dollar_token;
    let mut tokens = tokens.peekable();

    let Some(token) = tokens.peek() else {
        return error(dollar_token);
    };

    return match token {
        // $x
        TokenTree::Ident(_) => {
            parse_variable_binding_name(dollar_token, FieldMode::Single, &mut tokens)
        }

        // $$
        TokenTree::Punct(punct) if punct.as_char() == '$' => {
            let Some(TokenTree::Punct(punct)) = tokens.next() else {
                unreachable!()
            };
            Ok(FormalitySpecSymbol::Char { punct })
        }

        // $,x
        TokenTree::Punct(punct) if punct.as_char() == ',' => {
            tokens.next();
            parse_variable_binding_name(dollar_token, FieldMode::Comma, &mut tokens)
        }

        // $*x
        TokenTree::Punct(punct) if punct.as_char() == '*' => {
            tokens.next();
            parse_variable_binding_name(dollar_token, FieldMode::Many, &mut tokens)
        }

        // $?x
        TokenTree::Punct(punct) if punct.as_char() == '?' => {
            tokens.next();
            parse_variable_binding_name(dollar_token, FieldMode::Optional, &mut tokens)
        }

        // $<x> or $<?x>
        TokenTree::Punct(punct) if punct.as_char() == '<' => {
            tokens.next();

            // consume `x` or `?x`
            let result = parse_delimited(dollar_token, '<', '>', &mut tokens)?;

            // we should see a `>` next
            match tokens.next() {
                Some(TokenTree::Punct(punct)) if punct.as_char() == '>' => Ok(result),
                _ => error(dollar_token),
            }
        }

        // $(x) or $(?x)
        // $[x] or $[?x]
        // ${x} or ${?x}
        TokenTree::Group(_) => {
            let Some(TokenTree::Group(group)) = tokens.next() else {
                unreachable!()
            };
            let Some((open, close)) = open_close(&group) else {
                return error(&group);
            };

            // consume `x` or `?x`
            let mut group_tokens = group.stream().into_iter().peekable();
            let result = parse_delimited(dollar_token, open, close, &mut group_tokens)?;

            // there shouldn't be anything else in the token tree
            if let Some(t) = group_tokens.next() {
                return error(&t);
            }
            Ok(result)
        }

        _ => error(dollar_token),
    };

    fn parse_delimited(
        dollar_token: &Punct,
        open: char,
        close: char,
        tokens: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> syn::Result<FormalitySpecSymbol> {
        // Check for a `?` and consume it, if present.
        let optional = match tokens.peek() {
            Some(TokenTree::Punct(punct)) if punct.as_char() == '?' => {
                tokens.next(); // drop `?` character
                true
            }

            _ => false,
        };

        parse_variable_binding_name(
            dollar_token,
            FieldMode::DelimitedVec {
                open,
                optional,
                close,
            },
            tokens,
        )
    }

    fn parse_variable_binding_name(
        dollar_token: &Punct,
        mode: FieldMode,
        tokens: &mut impl Iterator<Item = TokenTree>,
    ) -> syn::Result<FormalitySpecSymbol> {
        // Extract the name of the field.
        let name = match tokens.next() {
            Some(TokenTree::Ident(name)) => name,
            _ => return error(dollar_token),
        };

        Ok(FormalitySpecSymbol::Field { name, mode })
    }

    fn error(at_token: &impl Spanned) -> syn::Result<FormalitySpecSymbol> {
        let message = "invalid field reference in grammar";
        Err(syn::Error::new(at_token.span(), message))
    }
}

fn open_close(g: &Group) -> Option<(char, char)> {
    match g.delimiter() {
        proc_macro2::Delimiter::Parenthesis => Some(('(', ')')),
        proc_macro2::Delimiter::Brace => Some(('{', '}')),
        proc_macro2::Delimiter::Bracket => Some(('[', ']')),
        proc_macro2::Delimiter::None => None,
    }
}
