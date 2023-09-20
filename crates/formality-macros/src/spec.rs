use proc_macro2::{Ident, Punct, TokenStream, TokenTree};

/// The "formality spec" guides parsing and serialization.
///
/// It has a very simple format consisting of a series of tokens:
///
/// * an identifier like `foo` is parsed as a keyword
/// * a field like `$foo` parses the type of the declared field
///     * you can also do `$*foo` to use the `parse_many` option
/// * a character like `<` is parsed as is; a group like `[..]` parses a `[`, the contents, and then `]`
pub struct FormalitySpec {
    pub ops: Vec<FormalitySpecOp>,
}

pub enum FormalitySpecOp {
    /// `$foo` or `$foo*` -- indicates we should parse the type of the given field.
    Field { name: Ident, mode: FieldMode },

    /// `foo` -- indicates we should parse the given keyword.
    Keyword { ident: Ident },

    /// `<` -- indicates we should parse the given char. We currently ignoring the spacing rules.
    Char { punct: Punct },

    /// Specific delimeter (e.g., `(`) we should parse.
    Delimeter { text: char },
}

pub enum FieldMode {
    Single,
    Many,
    Comma,
}

impl syn::parse::Parse for FormalitySpec {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let token_stream: TokenStream = input.parse()?;
        let mut ops = vec![];
        token_stream_to_ops(&mut ops, token_stream)?;
        Ok(FormalitySpec { ops })
    }
}

fn token_stream_to_ops(
    ops: &mut Vec<FormalitySpecOp>,
    token_stream: TokenStream,
) -> syn::Result<()> {
    use FormalitySpecOp::*;

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
                    ops.push(Delimeter { text: ch });
                }
                token_stream_to_ops(ops, v.stream())?;
                if let Some(ch) = close_text {
                    ops.push(Delimeter { text: ch });
                }
            }
            proc_macro2::TokenTree::Ident(ident) => ops.push(Keyword { ident }),
            proc_macro2::TokenTree::Punct(punct) => match punct.as_char() {
                '$' => ops.push(parse_variable_binding(punct, &mut tokens)?),
                _ => ops.push(Char { punct }),
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
) -> syn::Result<FormalitySpecOp> {
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
                '$' => return Ok(FormalitySpecOp::Char { punct }),
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

    Ok(FormalitySpecOp::Field { name, mode })
}
