use std::{str::FromStr, sync::Arc};

use crate::{
    cast::{To, Upcast},
    collections::Set,
    derive_links::{Fold, Parameter, ParameterKind},
    grammar::{Binder, BoundVar, KindedVarIndex},
};
use std::fmt::Debug;

mod test;

/// Parses `text` as a term with no bindings in scope.
#[track_caller]
pub fn term<T>(text: &str) -> T
where
    T: Parse,
{
    term_with(None::<(String, Parameter)>, text)
}

#[track_caller]
pub fn term_with<T, B>(bindings: impl IntoIterator<Item = B>, text: &str) -> T
where
    T: Parse,
    B: Upcast<(String, Parameter)>,
{
    let scope = Scope::new(bindings.into_iter().map(|b| b.upcast()));
    let (t, remainder) = match T::parse(&scope, text) {
        Some(v) => v,
        None => {
            panic!("failed to parse {text:?}");
        }
    };
    if !remainder.trim().is_empty() {
        panic!("extra tokens after parsing {text:?} to {t:?}: {remainder:?}");
    }
    t
}

pub trait Parse: Sized + Debug {
    fn parse<'t>(scope: &Scope, text: &'t str) -> Option<(Self, &'t str)>;

    fn parse_many<'t>(scope: &Scope, mut text: &'t str) -> (Vec<Self>, &'t str) {
        let mut result = vec![];
        while let Some((e, t)) = Self::parse(scope, text) {
            result.push(e);
            text = t;
        }
        (result, text)
    }

    /// Comma separated list with optional trailing comma.
    fn parse_comma<'t>(scope: &Scope, mut text: &'t str) -> (Vec<Self>, &'t str) {
        let mut result = vec![];
        while let Some((e, t)) = Self::parse(scope, text) {
            result.push(e);
            text = t;

            if let Some(t) = expect_char(',', text) {
                text = t;
            } else {
                break;
            }
        }

        (result, text)
    }
}

#[derive(Clone, Debug)]
pub struct Scope {
    bindings: Vec<(String, Parameter)>,
}

impl Scope {
    pub fn new(bindings: impl IntoIterator<Item = (String, Parameter)>) -> Self {
        Self {
            bindings: bindings.into_iter().collect(),
        }
    }

    pub fn lookup(&self, name: &str) -> Option<Parameter> {
        self.bindings
            .iter()
            .rev()
            .flat_map(|(n, p)| if name == n { Some(p.clone()) } else { None })
            .next()
    }

    pub fn with_bindings(&self, bindings: impl IntoIterator<Item = (String, Parameter)>) -> Self {
        let mut s = self.clone();
        s.bindings.extend(bindings);
        s
    }
}

#[derive(Clone, Debug)]
pub struct Binding {
    pub name: String,
    pub kvi: KindedVarIndex,
    pub bound_var: BoundVar,
}

impl<T> Parse for Vec<T>
where
    T: Parse,
{
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(scope: &Scope, text: &'t str) -> Option<(Self, &'t str)> {
        let text = expect_char('[', text)?;
        let (v, text) = T::parse_comma(scope, text);
        let text = expect_char(']', text)?;
        Some((v, text))
    }
}

impl<T> Parse for Set<T>
where
    T: Parse + Ord,
{
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(scope: &Scope, text: &'t str) -> Option<(Self, &'t str)> {
        let text = expect_char('{', text)?;
        let (v, text) = T::parse_comma(scope, text);
        let text = expect_char('}', text)?;
        let s = v.into_iter().collect();
        Some((s, text))
    }
}

impl<T> Parse for Option<T>
where
    T: Parse,
{
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(scope: &Scope, text: &'t str) -> Option<(Self, &'t str)> {
        match T::parse(scope, text) {
            Some((value, text)) => Some((Some(value), text)),
            None => Some((None, text)),
        }
    }
}

impl Parse for Binding {
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(scope: &Scope, text: &'t str) -> Option<(Self, &'t str)> {
        let (kind, text) = ParameterKind::parse(scope, text)?;
        let (name, text) = identifier(text)?;
        let (kvi, bound_var) = crate::grammar::fresh_bound_var(kind);
        Some((
            Binding {
                name,
                kvi,
                bound_var,
            },
            text,
        ))
    }
}

impl<T> Parse for Binder<T>
where
    T: Parse + Fold,
{
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(scope: &Scope, text: &'t str) -> Option<(Self, &'t str)> {
        let text = expect_char('<', text)?;
        let (bindings, text) = Binding::parse_comma(scope, text);
        let text = expect_char('>', text)?;

        // parse the contents with those names in scope
        let scope1 = scope.with_bindings(bindings.iter().map(|b| (b.name.clone(), b.kvi.to())));
        let (data, text) = T::parse(&scope1, text)?;

        let kvis: Vec<KindedVarIndex> = bindings.iter().map(|b| b.kvi).collect();
        Some((Binder::new(&kvis, data), text))
    }
}

impl<T> Parse for Arc<T>
where
    T: Parse,
{
    fn parse<'t>(scope: &Scope, text: &'t str) -> Option<(Self, &'t str)> {
        let (data, text) = T::parse(scope, text)?;
        Some((Arc::new(data), text))
    }
}

impl Parse for usize {
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(_scope: &Scope, text: &'t str) -> Option<(Self, &'t str)> {
        number(text)
    }
}

impl Parse for u32 {
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(_scope: &Scope, text: &'t str) -> Option<(Self, &'t str)> {
        number(text)
    }
}

impl Parse for u64 {
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(_scope: &Scope, text: &'t str) -> Option<(Self, &'t str)> {
        number(text)
    }
}

fn char(text: &str) -> Option<(char, &str)> {
    let ch = text.chars().next()?;
    Some((ch, &text[char::len_utf8(ch)..]))
}

#[tracing::instrument(level = "trace", ret)]
pub fn number<T>(text: &str) -> Option<(T, &str)>
where
    T: FromStr + Debug,
{
    let (id, text) = accumulate(text, char::is_numeric, char::is_numeric)?;
    let t = T::from_str(&id).ok()?;
    Some((t, text))
}

#[tracing::instrument(level = "trace", ret)]
pub fn expect_char(ch: char, text: &str) -> Option<&str> {
    let text = text.trim_start();
    let (ch1, text) = char(text)?;
    if ch == ch1 {
        Some(text)
    } else {
        None
    }
}

/// If `text` starts with `s`, return the remainder of `text`.
/// Don't use this for keywords, since if `s` is `"foo"`
/// and text is `"foobar"`, this would return `Some("bar")`.
#[tracing::instrument(level = "trace", ret)]
pub fn expect_str<'t>(s: &str, text: &'t str) -> Option<&'t str> {
    let text = text.trim_start();
    if text.starts_with(s) {
        Some(&text[s.len()..])
    } else {
        None
    }
}

/// Extracts a maximal identifier from the start of text,
/// following the usual rules.
#[tracing::instrument(level = "trace", ret)]
pub fn identifier(text: &str) -> Option<(String, &str)> {
    accumulate(
        text,
        |ch| match ch {
            'a'..='z' | 'A'..='Z' | '_' => true,
            _ => false,
        },
        |ch| match ch {
            'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => true,
            _ => false,
        },
    )
}

#[tracing::instrument(level = "trace", ret)]
pub fn expect_keyword<'t>(expected: &str, text: &'t str) -> Option<&'t str> {
    let (ident, text) = identifier(text)?;
    if &*ident == expected {
        Some(text)
    } else {
        None
    }
}

pub fn try_parse<R>(f: impl Fn() -> Option<R>) -> Option<R> {
    f()
}

pub fn require_unambiguous<'t, R>(f: impl IntoIterator<Item = (R, &'t str)>) -> Option<(R, &'t str)>
where
    R: std::fmt::Debug,
{
    let mut v: Vec<(R, &str)> = f.into_iter().collect();
    if v.len() > 1 {
        panic!("parsing ambiguity: {v:?}");
    } else {
        v.pop()
    }
}

/// Extracts a maximal identifier from the start of text,
/// following the usual rules.
fn accumulate(
    text: &str,
    start_test: impl Fn(char) -> bool,
    continue_test: impl Fn(char) -> bool,
) -> Option<(String, &str)> {
    let text = text.trim_start();
    let mut buffer = String::new();

    let (ch, text) = char(text)?;
    if !start_test(ch) {
        return None;
    }
    buffer.push(ch);

    let mut text = text;
    while let Some((ch, t)) = char(text) {
        if !continue_test(ch) {
            break;
        }

        buffer.push(ch);
        text = t;
    }

    Some((buffer, text))
}

impl<A: Parse, B: Parse> Parse for (A, B) {
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(scope: &Scope, text: &'t str) -> Option<(Self, &'t str)> {
        let text = expect_char('(', text)?;
        let (a, text) = A::parse(scope, text)?;
        let text = expect_char(',', text)?;
        let (b, text) = B::parse(scope, text)?;
        let text = expect_char(',', text).unwrap_or(text);
        let text = expect_char(')', text)?;
        Some(((a, b), text))
    }
}

impl<A: Parse, B: Parse, C: Parse> Parse for (A, B, C) {
    #[tracing::instrument(level = "trace", ret)]
    fn parse<'t>(scope: &Scope, text: &'t str) -> Option<(Self, &'t str)> {
        let text = expect_char('(', text)?;
        let (a, text) = A::parse(scope, text)?;
        let text = expect_char(',', text)?;
        let (b, text) = B::parse(scope, text)?;
        let text = expect_char(',', text)?;
        let (c, text) = C::parse(scope, text)?;
        let text = expect_char(',', text).unwrap_or(text);
        let text = expect_char(')', text)?;
        Some(((a, b, c), text))
    }
}
