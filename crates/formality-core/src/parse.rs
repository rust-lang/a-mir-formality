use std::{str::FromStr, sync::Arc};

use crate::{
    binder::{fresh_bound_var, CoreBinder},
    cast::To,
    collections::Set,
    language::{CoreKind, CoreParameter, Language},
    set,
    term::CoreTerm,
    variable::CoreBoundVar,
};
use std::fmt::Debug;

/// Trait for parsing a [`Term<L>`](`crate::term::Term`) as input.
pub trait CoreParse<L: Language>: Sized + Debug {
    /// Parse a single instance of this type, returning an error if no such
    /// instance is present.
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self>;

    /// Parse many instances of self, expecting `close_char` to appear after the last instance
    /// (`close_char` is not consumed).
    fn parse_many<'t>(
        scope: &Scope<L>,
        mut text: &'t str,
        close_char: char,
    ) -> ParseResult<'t, Vec<Self>> {
        let mut result = vec![];
        while !skip_whitespace(text).starts_with(close_char) {
            let (e, t) = Self::parse(scope, text)?;
            result.push(e);
            text = t;
        }
        Ok((result, text))
    }

    /// Comma separated list with optional trailing comma.
    fn parse_comma<'t>(
        scope: &Scope<L>,
        mut text: &'t str,
        close_char: char,
    ) -> ParseResult<'t, Vec<Self>> {
        let mut result = vec![];
        while !skip_whitespace(text).starts_with(close_char) {
            let (e, t) = Self::parse(scope, text)?;
            result.push(e);
            text = t;

            if let Ok(((), t)) = expect_char(',', text) {
                text = t;
            } else {
                break;
            }
        }

        Ok((result, text))
    }
}

/// Tracks an error that occurred while parsing.
/// The parse error records the input text it saw, which will be
/// some suffix of the original input, along with a message.
///
/// The actual [`ParseResult`] type tracks a *set* of parse errors.
/// When parse errors are generated, there is just one (e.g., "expected identifier"),
/// but when there are choice points in the grammar (e.g., when parsing an enum),
/// those errors can be combined by [`require_unambiguous`].
#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct ParseError<'t> {
    /// Input that triggered the parse error. Some suffix
    /// of the original input.
    pub text: &'t str,

    /// Message describing what was expected.
    pub message: String,
}

impl<'t> ParseError<'t> {
    /// Creates a single parse error at the given point. Returns
    /// a set so that it can be wrapped as a [`ParseResult`].
    pub fn at(text: &'t str, message: String) -> Set<Self> {
        set![ParseError { text, message }]
    }

    /// Offset of this error relative to the starting point `text`
    pub fn offset(&self, text: &str) -> usize {
        assert!(text.ends_with(self.text));
        text.len() - self.text.len()
    }

    /// Returns the text that was consumed before this error occurred,
    /// with `text` is the starting point.
    pub fn consumed_before<'s>(&self, text: &'s str) -> &'s str {
        let o = self.offset(text);
        &text[..o]
    }
}

pub type ParseResult<'t, T> = Result<(T, &'t str), Set<ParseError<'t>>>;

/// Tracks the variables in scope at this point in parsing.
#[derive(Clone, Debug)]
pub struct Scope<L: Language> {
    bindings: Vec<(String, CoreParameter<L>)>,
}

impl<L: Language> Scope<L> {
    /// Creates a new scope with the given set of bindings.
    pub fn new(bindings: impl IntoIterator<Item = (String, CoreParameter<L>)>) -> Self {
        Self {
            bindings: bindings.into_iter().collect(),
        }
    }

    /// Look for a variable with the given name.
    pub fn lookup(&self, name: &str) -> Option<CoreParameter<L>> {
        self.bindings
            .iter()
            .rev()
            .flat_map(|(n, p)| if name == n { Some(p.clone()) } else { None })
            .next()
    }

    /// Create a new scope that extends `self` with `bindings`.
    pub fn with_bindings(
        &self,
        bindings: impl IntoIterator<Item = (String, CoreParameter<L>)>,
    ) -> Self {
        let mut s = self.clone();
        s.bindings.extend(bindings);
        s
    }
}

/// Records a single binding, used when parsing [`Binder`].
#[derive(Clone, Debug)]
pub struct Binding<L: Language> {
    /// Name the user during during parsing
    pub name: String,

    /// The bound var representation.
    pub bound_var: CoreBoundVar<L>,
}

impl<L, T> CoreParse<L> for Vec<T>
where
    L: Language,
    T: CoreParse<L>,
{
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        let ((), text) = expect_char('[', text)?;
        let (v, text) = T::parse_comma(scope, text, ']')?;
        let ((), text) = expect_char(']', text)?;
        Ok((v, text))
    }
}

impl<L, T> CoreParse<L> for Set<T>
where
    L: Language,
    T: CoreParse<L> + Ord,
{
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        let ((), text) = expect_char('{', text)?;
        let (v, text) = T::parse_comma(scope, text, '}')?;
        let ((), text) = expect_char('}', text)?;
        let s = v.into_iter().collect();
        Ok((s, text))
    }
}

impl<L, T> CoreParse<L> for Option<T>
where
    L: Language,
    T: CoreParse<L>,
{
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        match T::parse(scope, text) {
            Ok((value, text)) => Ok((Some(value), text)),
            Err(_) => Ok((None, text)),
        }
    }
}

/// Binding grammar is `$kind $name`, e.g., `ty Foo`.
impl<L: Language> CoreParse<L> for Binding<L> {
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        let (kind, text) = <CoreKind<L>>::parse(scope, text)?;
        let (name, text) = identifier(text)?;
        let bound_var = fresh_bound_var(kind);
        Ok((Binding { name, bound_var }, text))
    }
}

/// Parse a binder: find the names in scope, parse the contents, and then
/// replace names with debruijn indices.
impl<L, T> CoreParse<L> for CoreBinder<L, T>
where
    L: Language,
    T: CoreTerm<L>,
{
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        let ((), text) = expect_char(L::BINDING_OPEN, text)?;
        let (bindings, text) = Binding::parse_comma(scope, text, '>')?;
        let ((), text) = expect_char(L::BINDING_CLOSE, text)?;

        // parse the contents with those names in scope
        let scope1 =
            scope.with_bindings(bindings.iter().map(|b| (b.name.clone(), b.bound_var.to())));
        let (data, text) = T::parse(&scope1, text)?;

        let kvis: Vec<CoreBoundVar<L>> = bindings.iter().map(|b| b.bound_var).collect();
        Ok((CoreBinder::new(kvis, data), text))
    }
}

impl<L, T> CoreParse<L> for Arc<T>
where
    L: Language,
    T: CoreParse<L>,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        let (data, text) = T::parse(scope, text)?;
        Ok((Arc::new(data), text))
    }
}

impl<L> CoreParse<L> for usize
where
    L: Language,
{
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(_scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        number(text)
    }
}

impl<L> CoreParse<L> for u32
where
    L: Language,
{
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(_scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        number(text)
    }
}

impl<L> CoreParse<L> for u64
where
    L: Language,
{
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(_scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        number(text)
    }
}

/// Extract the next character from input, returning an error if we've reached the input.
///
/// Warning: does not skip whitespace.
fn char(text: &str) -> ParseResult<'_, char> {
    let ch = match text.chars().next() {
        Some(c) => c,
        None => return Err(ParseError::at(text, "unexpected end of input".to_string())),
    };
    Ok((ch, &text[char::len_utf8(ch)..]))
}

/// Extract a number from the input, erroring if the input does not start with a number.
#[tracing::instrument(level = "trace", ret)]
pub fn number<T>(text0: &str) -> ParseResult<'_, T>
where
    T: FromStr + Debug,
{
    let (id, text1) = accumulate(text0, char::is_numeric, char::is_numeric, "number")?;
    match T::from_str(&id) {
        Ok(t) => Ok((t, text1)),
        Err(_) => Err(ParseError::at(text0, format!("invalid number"))),
    }
}

/// Consume next character and require that it be `ch`.
#[tracing::instrument(level = "trace", ret)]
pub fn expect_char(ch: char, text0: &str) -> ParseResult<'_, ()> {
    let text1 = skip_whitespace(text0);
    let (ch1, text1) = char(text1)?;
    if ch == ch1 {
        Ok(((), text1))
    } else {
        Err(ParseError::at(text0, format!("expected `{}`", ch)))
    }
}

/// Consume a comma if one is present.
#[tracing::instrument(level = "trace", ret)]
pub fn skip_trailing_comma(text: &str) -> &str {
    text.strip_prefix(',').unwrap_or(text)
}

/// Extracts a maximal identifier from the start of text,
/// following the usual rules.
#[tracing::instrument(level = "trace", ret)]
pub fn identifier(text: &str) -> ParseResult<'_, String> {
    accumulate(
        text,
        |ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '_'),
        |ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'),
        "identifier",
    )
}

/// Consume next identifier, requiring that it be equal to `expected`.
#[tracing::instrument(level = "trace", ret)]
pub fn expect_keyword<'t>(expected: &str, text0: &'t str) -> ParseResult<'t, ()> {
    match identifier(text0) {
        Ok((ident, text1)) if &*ident == expected => Ok(((), text1)),
        _ => Err(ParseError::at(text0, format!("expected `{}`", expected))),
    }
}

/// Reject next identifier if it is the given keyword. Consumes nothing.
#[tracing::instrument(level = "trace", ret)]
pub fn reject_keyword<'t>(expected: &str, text0: &'t str) -> ParseResult<'t, ()> {
    match expect_keyword(expected, text0) {
        Ok(_) => Err(ParseError::at(
            text0,
            format!("found keyword `{}`", expected),
        )),
        Err(_) => Ok(((), text0)),
    }
}

/// Convenience function for use when generating code: calls the closure it is given
/// as argument. Used to introduce new scope for name bindings.
pub fn try_parse<'a, R>(f: impl Fn() -> ParseResult<'a, R>) -> ParseResult<'a, R> {
    f()
}

/// Used at choice points in the grammar. Iterates over all possible parses, looking
/// for a single successful parse. If there are multiple successful parses, that
/// indicates an ambiguous grammar, so we panic. If there are no successful parses,
/// tries to come up with the best error it can: it prefers errors that arise from "partially successful"
/// parses (e.g., parses that consume some input before failing), but if there are none of those,
/// it will give an error at `text` saying that we expected to find a `expected`.
pub fn require_unambiguous<'t, R>(
    text: &'t str,
    f: impl IntoIterator<Item = ParseResult<'t, R>>,
    expected: &'static str,
) -> ParseResult<'t, R>
where
    R: std::fmt::Debug,
{
    let mut errors = set![];
    let mut results = vec![];
    for result in f {
        match result {
            Ok(v) => results.push(v),
            Err(es) => {
                for e in es {
                    // only include an error if the error resulted after at least
                    // one non-whitespace character was consumed
                    if !skip_whitespace(e.consumed_before(text)).is_empty() {
                        errors.insert(e);
                    }
                }
            }
        }
    }

    if results.len() > 1 {
        // More than one *positive* result indicates an ambiguous grammar, which is a programmer bug,
        // not a fault of the input, so we panic (rather than returning Err)
        panic!("parsing ambiguity: {results:?}");
    } else if results.len() == 1 {
        Ok(results.pop().unwrap())
    } else if errors.is_empty() {
        Err(ParseError::at(text, format!("{} expected", expected)))
    } else {
        Err(errors)
    }
}

/// Extracts a maximal identifier from the start of text,
/// following the usual rules.
fn accumulate<'t>(
    text0: &'t str,
    start_test: impl Fn(char) -> bool,
    continue_test: impl Fn(char) -> bool,
    description: &'static str,
) -> ParseResult<'t, String> {
    let text1 = skip_whitespace(text0);
    let mut buffer = String::new();

    let (ch, text1) = char(text1)?;
    if !start_test(ch) {
        return Err(ParseError::at(text0, format!("{} expected", description)));
    }
    buffer.push(ch);

    let mut text1 = text1;
    while let Ok((ch, t)) = char(text1) {
        if !continue_test(ch) {
            break;
        }

        buffer.push(ch);
        text1 = t;
    }

    Ok((buffer, text1))
}

impl<L: Language, A: CoreParse<L>, B: CoreParse<L>> CoreParse<L> for (A, B) {
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        let ((), text) = expect_char('(', text)?;
        let (a, text) = A::parse(scope, text)?;
        let ((), text) = expect_char(',', text)?;
        let (b, text) = B::parse(scope, text)?;
        let text = skip_trailing_comma(text);
        let ((), text) = expect_char(')', text)?;
        Ok(((a, b), text))
    }
}

impl<L: Language> CoreParse<L> for () {
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        let ((), text) = expect_char('(', text)?;
        let ((), text) = expect_char(')', text)?;
        Ok(((), text))
    }
}

impl<L: Language, A: CoreParse<L>, B: CoreParse<L>, C: CoreParse<L>> CoreParse<L> for (A, B, C) {
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        let ((), text) = expect_char('(', text)?;
        let (a, text) = A::parse(scope, text)?;
        let ((), text) = expect_char(',', text)?;
        let (b, text) = B::parse(scope, text)?;
        let ((), text) = expect_char(',', text)?;
        let (c, text) = C::parse(scope, text)?;
        let text = skip_trailing_comma(text);
        let ((), text) = expect_char(')', text)?;
        Ok(((a, b, c), text))
    }
}

/// Skips leading whitespace and comments.
pub fn skip_whitespace(mut text: &str) -> &str {
    loop {
        let len = text.len();

        text = text.trim_start();

        if text.starts_with("//") {
            match text.find('\n') {
                Some(index) => {
                    text = &text[index + 1..];
                }
                None => {
                    text = "";
                }
            }
        }

        if text.len() == len {
            return text;
        }
    }
}
