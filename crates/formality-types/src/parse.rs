use crate::{
    derive_links::{Fold, Parameter, ParameterKind},
    grammar::{Binder, BoundVar, KindedVarIndex},
};

pub trait Parse: Sized {
    fn parse<'t>(scope: &Scope, text: &'t str) -> Option<(Self, &'t str)>;

    fn parse_many<'t>(scope: &Scope, mut text: &'t str) -> (Vec<Self>, &'t str) {
        let mut result = vec![];
        while let Some((e, t)) = Self::parse(scope, text) {
            result.push(e);
            text = t;
        }
        (result, text)
    }
}

#[derive(Clone)]
pub struct Scope {
    bindings: Vec<Binding>,
}

impl Scope {
    pub fn lookup(&self, name: &str) -> Option<Parameter> {
        for binding in self.bindings.iter().rev() {
            if binding.name == name {
                return Some(binding.bound_var.into_parameter(binding.kvi.kind));
            }
        }
        None
    }

    pub fn with_bindings(&self, bindings: &[Binding]) -> Self {
        let mut s = self.clone();
        s.bindings.extend(bindings.iter().cloned());
        s
    }
}

#[derive(Clone)]
pub struct Binding {
    pub name: String,
    pub kvi: KindedVarIndex,
    pub bound_var: BoundVar,
}

impl<T> Parse for Vec<T>
where
    T: Parse,
{
    fn parse<'t>(scope: &Scope, mut text: &'t str) -> Option<(Self, &'t str)> {
        text = expect(text, '[')?;
        let (v, text) = T::parse_many(scope, text);
        text = expect(text, ']')?;
        Some((v, text))
    }
}

impl Parse for Binding {
    fn parse<'t>(scope: &Scope, text: &'t str) -> Option<(Self, &'t str)> {
        let text = expect(text, '(')?;
        let (kind, text) = ParameterKind::parse(scope, text)?;
        let (name, text) = identifier(text)?;
        let text = expect(text, ')')?;
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
    fn parse<'t>(scope: &Scope, text: &'t str) -> Option<(Self, &'t str)> {
        let text = expect(text, '<')?;
        let (bindings, text) = Binding::parse_many(scope, text);
        let text = expect(text, '>')?;

        // parse the contents with those names in scope
        let scope1 = scope.with_bindings(&bindings);
        let (data, text) = T::parse(&scope1, text)?;

        let kvis: Vec<KindedVarIndex> = bindings.iter().map(|b| b.kvi).collect();
        Some((Binder::new(&kvis, data), text))
    }
}

pub fn char(s: &str) -> Option<(char, &str)> {
    let ch = s.chars().next()?;
    Some((ch, &s[char::len_utf8(ch)..]))
}

pub fn expect(s: &str, ch: char) -> Option<&str> {
    let (ch1, s) = char(s)?;
    if ch == ch1 {
        Some(s)
    } else {
        None
    }
}

pub fn identifier(mut text: &str) -> Option<(String, &str)> {
    let mut buffer = String::new();

    let (ch, text) = char(text)?;
    if !ch.is_alphabetic() {
        return None;
    }
    buffer.push(ch);

    while let Some((ch, t)) = char(text) {
        if !ch.is_alphanumeric() {
            break;
        }

        buffer.push(ch);
        text = t;
    }

    Some((buffer, text))
}

pub fn expect_keyword<'t>(mut text: &'t str, expected: &str) -> Option<&'t str> {
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
