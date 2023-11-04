use proc_macro2::{Ident, Punct, TokenStream, TokenTree};

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
                let (open_text, close_text) = match v.delimiter() {
                    proc_macro2::Delimiter::Parenthesis => (Some('('), Some(')')),
                    proc_macro2::Delimiter::Brace => (Some('{'), Some('}')),
                    proc_macro2::Delimiter::Bracket => (Some('['), Some(']')),
                    proc_macro2::Delimiter::None => (None, None),
                };

                if let Some(ch) = open_text {
                    symbols.push(Delimeter { text: ch });
                }
                token_stream_to_symbols(symbols, v.stream())?;
                if let Some(ch) = close_text {
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
    let error = || {
        let message = "expected field name or field mode (`,`, `*`)";
        Err(syn::Error::new(dollar_token.span(), message))
    };

    let mut next_token = match tokens.next() {
        Some(v) => v,
        None => return error(),
    };

    // If there is a field mode (e.g., `*`) present, parse it.
    let mode = match next_token {
        TokenTree::Ident(_) => FieldMode::Single,
        TokenTree::Punct(punct) => {
            let mode = match punct.as_char() {
                ',' => FieldMode::Comma,
                '*' => FieldMode::Many,
                '?' => FieldMode::Optional,
                '$' => return Ok(FormalitySpecSymbol::Char { punct }),
                _ => return error(),
            };

            next_token = match tokens.next() {
                Some(v) => v,
                None => return error(),
            };

            mode
        }
        TokenTree::Group(_) | TokenTree::Literal(_) => return error(),
    };

    // Extract the name of the field.
    let name = match next_token {
        TokenTree::Ident(name) => name,
        _ => return error(),
    };

    Ok(FormalitySpecSymbol::Field { name, mode })
}
