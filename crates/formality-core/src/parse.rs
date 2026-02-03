use std::sync::Arc;

use crate::{
    binder::CoreBinder,
    cast_impl,
    collections::Set,
    language::{CoreKind, Language},
    set,
    term::CoreTerm,
    variable::{CoreBoundVar, CoreVariable},
    Fallible, Upcast, UpcastFrom, Upcasted,
};
use std::fmt::Debug;

/// Trait for parsing a [`Term<L>`](`crate::term::Term`) as input.
/// Typically this is auto-generated with the `#[term]` procedural macro,
/// but you can implement it by hand if you want a very customized parse.
pub trait CoreParse<L: Language>: Sized + Debug + Clone + Eq + 'static + Upcast<Self> {
    /// Parse a single instance of this type, returning an error if no such
    /// instance is present.
    ///
    /// This is intended to be implemented by constructing
    /// a [`Parser`][], typically with `Parser::single_variant_nonterminal`.
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self>;
}

mod parser;
pub use parser::{skip_whitespace, ActiveVariant, Parser, Precedence};

/// Parses `text` as a term with the given bindings in scope.
///
/// References to the given string will be replaced with the given parameter
/// when parsing types, lifetimes, etc.
#[track_caller]
pub fn core_term_with<L, T, B>(bindings: impl IntoIterator<Item = B>, text: &str) -> Fallible<T>
where
    T: CoreParse<L>,
    L: Language,
    B: Upcast<(String, CoreVariable<L>)>,
{
    let scope = Scope::new(bindings.into_iter().map(|b| b.upcast()));
    let parse = match T::parse(&scope, text) {
        Ok(v) => v,
        Err(errors) => {
            let mut err = crate::anyhow!("failed to parse {text}");
            for error in errors {
                err = err.context(error.text.to_owned()).context(error.message);
            }
            return Err(err);
        }
    };
    let (value, remainder) = parse.finish();

    if !skip_whitespace(remainder).is_empty() {
        crate::bail!("extra tokens after parsing {text:?} to {value:?}: {remainder:?}");
    }

    Ok(value)
}

/// Record from a successful parse.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SuccessfulParse<'t, T> {
    /// The new point in the input, after we've consumed whatever text we have.
    text: &'t str,

    /// A list of reductions we had to perform. *Reductions* are the names
    /// of enum variants that we have produced. For example, given an enum like `Foo { Bar(String, String) }`,
    /// and an input `foo("hello", "world")`, the reductions that result would be
    /// `["String", "String", "Foo::Bar"]`. This is used to resolve ambiguities
    /// more successfully.
    ///
    /// Important: reductions are only recorded if they represent a significant
    /// change. "is-a" relationships do not result in reductions. For example,
    /// given `enum Expr { #[cast] Base(BaseExpr), ... }`, only the reductions
    /// from `BaseExpr` would be recorded, there would be no `Expr::Base`
    /// reduction.
    reductions: Vec<(&'static str, ReductionKind)>,

    /// The precedence of this parse, which is derived from the value given
    /// to `parse_variant`.
    precedence: Precedence,

    /// The value produced.
    value: T,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum ReductionKind {
    #[default]
    Normal,

    /// A "cast" reduction is created by invoking `Parser::parse_variant_cast`
    /// or from the `#[cast]` variants in an enum. This kind of reduction
    /// represents an "is-a" relationship and does not add semantic meaning.
    ///
    /// Cast variants interact differently with ambiguity detection.
    /// Consider this grammar:
    ///
    /// ```text
    /// X = Y | Z // X has two variants
    /// Y = A     // Y has 1 variant
    /// Z = A B   // Z has 1 variant
    /// A = "a"   // A has 1 variant
    /// B = "b"   // B has 1 variant
    /// ```
    ///
    /// If you mark the two `X` variants (`X = Y` and `X = Z`)
    /// as cast variants, then the input `"a b"` is considered
    /// unambiguous and is parsed as `X = (Z = (A = "a') (B = "b))`
    /// with no remainder.
    ///
    /// If you don't mark those variants as cast variants,
    /// then we consider this *ambiguous*, because
    /// it could be that you want `X = (Y = (A = "a"))` with
    /// a remainder of `"b"`. This is appropriate
    /// if choosing Y vs Z has different semantic meaning.
    Cast,

    /// A "variable" reduction results from invoking
    /// `Parser::parse_variant_variable` to parse an in-scope variable
    /// or from a `#[variable]` variant in an enum.
    ///
    /// In general we prefer to interpret
    /// an identifier as an in-scope variable if there is one
    /// (see the test `parser-var-id-ambiguity` for examples).
    Variable,

    /// A "identifier" reduction results from invoking
    /// `Parser::parse_variant_identifier` to parse an identifier
    /// or from parsing a type declared with the `formality_core::id!`
    /// macro.
    ///
    /// In general we prefer to interpret
    /// an identifier as an in-scope variable if there is one
    /// (see the test `parser-var-id-ambiguity` for examples).
    Identifier,
}

impl<'t, T> SuccessfulParse<'t, T> {
    /// Extract the value parsed and the remaining text,
    /// ignoring the reductions.
    pub fn finish(self) -> (T, &'t str) {
        (self.value, self.text)
    }

    /// Maps the value using `op` -- this is meant for 'no-op' conversions like going from `T` to `Arc<T>`,
    /// and hence the list of reductions does not change.
    pub fn map<U>(self, op: impl FnOnce(T) -> U) -> SuccessfulParse<'t, U> {
        SuccessfulParse {
            text: self.text,
            reductions: self.reductions,
            precedence: self.precedence,
            value: op(self.value),
        }
    }
}

impl<'t, T, U> UpcastFrom<SuccessfulParse<'t, T>> for SuccessfulParse<'t, U>
where
    T: Upcast<U>,
    T: Clone,
{
    fn upcast_from(term: SuccessfulParse<'t, T>) -> Self {
        SuccessfulParse {
            text: term.text,
            reductions: term.reductions,
            precedence: term.precedence,
            value: term.value.upcast(),
        }
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
        let text = parser::skip_whitespace(text);
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

    /// True if any tokens were consumed before this error occurred,
    /// with `text` as the starting point of the parse.
    pub fn consumed_any_since(&self, text: &str) -> bool {
        !skip_whitespace(self.consumed_before(text)).is_empty()
    }
}

pub type ParseResult<'t, T> = Result<SuccessfulParse<'t, T>, Set<ParseError<'t>>>;
pub type TokenResult<'t, T> = Result<(T, &'t str), Set<ParseError<'t>>>;

/// Tracks the variables in scope at this point in parsing.
#[derive(Clone, Debug, Default)]
pub struct Scope<L: Language> {
    bindings: Vec<(String, CoreVariable<L>)>,
}

impl<L: Language> Scope<L> {
    /// Creates a new scope with the given set of bindings.
    pub fn new(bindings: impl IntoIterator<Item = (String, CoreVariable<L>)>) -> Self {
        Self {
            bindings: bindings.into_iter().collect(),
        }
    }

    /// Look for a variable with the given name.
    pub fn lookup(&self, name: &str) -> Option<CoreVariable<L>> {
        self.bindings
            .iter()
            .rev()
            .flat_map(|(n, p)| if name == n { Some(*p) } else { None })
            .next()
    }

    /// Create a new scope that extends `self` with `bindings`.
    pub fn with_bindings(
        &self,
        bindings: impl IntoIterator<Item = impl Upcast<(String, CoreVariable<L>)>>,
    ) -> Self {
        let mut s = self.clone();
        s.bindings.extend(bindings.into_iter().upcasted());
        s
    }
}

/// Records a single binding, used when parsing [`Binder`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Binding<L: Language> {
    /// Name the user during during parsing
    pub name: String,

    /// The bound var representation.
    pub bound_var: CoreBoundVar<L>,
}

cast_impl!(impl(L: Language) Binding<L>);

impl<L, T> CoreParse<L> for Vec<T>
where
    L: Language,
    T: CoreParse<L>,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "Vec", |p| {
            p.delimited_nonterminal('[', false, ']')
        })
    }
}

impl<L, T> CoreParse<L> for Set<T>
where
    L: Language,
    T: CoreParse<L> + Ord,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "Set", |p| {
            p.delimited_nonterminal('{', false, '}')
        })
    }
}

impl<L, T> CoreParse<L> for Option<T>
where
    L: Language,
    T: CoreParse<L>,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "Option", |p| p.opt_nonterminal())
    }
}

/// Binding grammar is `$kind $name`, e.g., `ty Foo`.
impl<L: Language> CoreParse<L> for Binding<L> {
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "Binding", |p| {
            let kind: CoreKind<L> = p.nonterminal()?;
            let name = p.identifier()?;
            let bound_var = CoreBoundVar::fresh(kind);
            Ok(Binding { name, bound_var })
        })
    }
}

/// Parse a binder: find the names in scope, parse the contents, and then
/// replace names with debruijn indices.
impl<L, T> CoreParse<L> for CoreBinder<L, T>
where
    L: Language,
    T: CoreTerm<L>,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "Binder", |p| {
            match p.expect_char(L::BINDING_OPEN) {
                Ok(()) => {}
                Err(_) => {
                    return Ok(CoreBinder::dummy(p.nonterminal()?));
                }
            }

            let bindings: Vec<Binding<L>> = p.comma_nonterminal()?;
            p.expect_char(L::BINDING_CLOSE)?;

            let data: T = p.with_scope(
                scope.with_bindings(bindings.iter().map(|b| (&b.name, b.bound_var))),
                |p| p.nonterminal(),
            )?;

            let kvis: Vec<CoreBoundVar<L>> = bindings.iter().map(|b| b.bound_var).collect();
            Ok(CoreBinder::new(kvis, data))
        })
    }
}

impl<L, T> CoreParse<L> for Arc<T>
where
    L: Language,
    T: CoreParse<L>,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        T::parse(scope, text).map(|success| success.map(Arc::new))
    }
}

impl<L> CoreParse<L> for bool
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::multi_variant(scope, text, "bool", |parser| {
            parser.parse_variant("true", Precedence::default(), |p| {
                p.expect_keyword("true")?;
                Ok(true)
            });

            parser.parse_variant("false", Precedence::default(), |p| {
                p.expect_keyword("false")?;
                Ok(false)
            });
        })
    }
}

impl<L> CoreParse<L> for usize
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "usize", |p| p.number())
    }
}

impl<L> CoreParse<L> for u8
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "u8", |p| p.number())
    }
}

impl<L> CoreParse<L> for u16
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "u16", |p| p.number())
    }
}

impl<L> CoreParse<L> for u32
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "u32", |p| p.number())
    }
}

impl<L> CoreParse<L> for u64
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "u64", |p| p.number())
    }
}

impl<L> CoreParse<L> for i8
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "u16", |p| p.number())
    }
}

impl<L> CoreParse<L> for i16
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "u16", |p| p.number())
    }
}

impl<L> CoreParse<L> for i32
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "u16", |p| p.number())
    }
}

impl<L> CoreParse<L> for i64
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "u16", |p| p.number())
    }
}

impl<L> CoreParse<L> for isize
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "u16", |p| p.number())
    }
}

impl<L: Language> CoreParse<L> for () {
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "`()`", |p| {
            p.expect_char('(')?;
            p.expect_char(')')?;
            Ok(())
        })
    }
}

impl<L: Language, A: CoreParse<L>, B: CoreParse<L>> CoreParse<L> for (A, B) {
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "tuple", |p| {
            p.expect_char('(')?;
            let a: A = p.nonterminal()?;
            p.expect_char(',')?;
            let b: B = p.nonterminal()?;
            p.skip_trailing_comma();
            p.expect_char(')')?;
            Ok((a, b))
        })
    }
}

impl<L: Language, A: CoreParse<L>, B: CoreParse<L>, C: CoreParse<L>> CoreParse<L> for (A, B, C) {
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "tuple", |p| {
            p.expect_char('(')?;
            let a: A = p.nonterminal()?;
            p.expect_char(',')?;
            let b: B = p.nonterminal()?;
            p.expect_char(',')?;
            let c: C = p.nonterminal()?;
            p.skip_trailing_comma();
            p.expect_char(')')?;
            Ok((a, b, c))
        })
    }
}

impl<L: Language> CoreParse<L> for CoreVariable<L> {
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "variable", |p| p.variable())
    }
}
