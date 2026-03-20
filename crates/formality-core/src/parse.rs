use std::sync::Arc;

use crate::{
    binder::CoreBinder,
    cast_impl,
    collections::Set,
    language::{CoreKind, Language},
    parse::parser::consumed_any_since,
    set,
    term::CoreTerm,
    variable::{CoreBoundVar, CoreVariable},
    Fallible, Upcast, UpcastFrom, Upcasted,
};
use std::fmt::Debug;

/// Trait for parsing a [`Term<L>`](`crate::term::Term`) as input.
/// Typically this is auto-generated with the `#[term]` procedural macro,
/// but you can implement it by hand if you want a very customized parse.
pub trait CoreParse<L: Language>: ParseSuccessType + Upcast<Self> {
    /// Parse a single instance of this type, returning an error if no such
    /// instance is present.
    ///
    /// This is intended to be implemented by constructing
    /// a [`Parser`][], typically with `Parser::single_variant_nonterminal`.
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self>;
}

/// Trait for defining how bindings are parsed in a language.
/// Implement this on `L::Parameter` to control the binding syntax.
/// The default behavior (`$kind $name`, e.g., `ty Foo`) can be
/// reused via [`default_binding_parse`].
pub trait CoreParseBinding<L: Language> {
    fn parse_binding<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Binding<L>>;

    /// Parse a variable name from input. Called by `variable()` to extract
    /// the name string that will be looked up in scope.
    ///
    /// The default implementation parses a standard identifier.
    /// Override this to support alternative variable syntaxes
    /// (e.g., tick-prefixed lifetimes like `'a`).
    fn parse_variable_name<'t>(
        p: &mut ActiveVariant<'_, 't, L>,
    ) -> Result<String, Set<ParseError<'t>>> {
        p.identifier()
    }
}

/// Default binding parse: `$kind $name`, e.g., `ty Foo`.
/// Languages can call this from their [`CoreParseBinding`] impl
/// to get the standard behavior.
pub fn default_binding_parse<'t, L: Language>(
    scope: &Scope<L>,
    text: &'t str,
) -> ParseResult<'t, Binding<L>> {
    Parser::single_variant(scope, text, "Binding", |p| {
        p.each_nonterminal(|kind: CoreKind<L>, p| {
            let name = p.identifier()?;
            let bound_var = CoreBoundVar::fresh(kind);
            p.ok(Binding { name, bound_var })
        })
    })
}

mod parser;
pub use parser::{skip_whitespace, ActiveVariant, Parser, Precedence};

/// A diagnostic for parse failures, rendered with source context
/// and nonterminal backtrace using miette.
#[derive(Debug)]
struct ParseDiagnostic {
    /// The full source text being parsed.
    source_code: String,

    /// The error message (e.g., "expected `}`").
    message: String,

    /// Byte offset of the error point in the source.
    error_offset: usize,

    /// The nonterminal backtrace frames, converted to byte offsets.
    frames: Vec<DiagnosticFrame>,
}

#[derive(Debug)]
struct DiagnosticFrame {
    name: &'static str,
    offset: usize,
}

impl ParseDiagnostic {
    /// Create a diagnostic from a ParseError and the full input text.
    fn from_error(error: &ParseError<'_>, full_input: &str) -> Self {
        let error_offset = error.offset(full_input);
        let frames: Vec<DiagnosticFrame> = error
            .backtrace
            .iter()
            .map(|frame| {
                let offset = full_input.len() - frame.remaining_len;
                DiagnosticFrame {
                    name: frame.name,
                    offset,
                }
            })
            .collect();

        ParseDiagnostic {
            source_code: full_input.to_owned(),
            message: error.message.clone(),
            error_offset,
            frames,
        }
    }

    /// Render using miette's GraphicalReportHandler to produce
    /// a string with source context, labeled spans, and backtrace.
    fn render(self) -> String {
        use miette::{GraphicalReportHandler, GraphicalTheme, LabeledSpan};

        let mut labels = Vec::new();

        // Add backtrace frames as context labels (outermost first)
        for frame in &self.frames {
            labels.push(LabeledSpan::at_offset(
                frame.offset,
                format!("while parsing {}", frame.name),
            ));
        }

        // The error point itself (added last so it renders as primary)
        labels.push(LabeledSpan::at_offset(self.error_offset, &self.message));

        let report =
            miette::miette!(labels = labels, "{}", self.message).with_source_code(self.source_code);

        let mut out = String::new();
        GraphicalReportHandler::new()
            .with_theme(GraphicalTheme::unicode_nocolor())
            .render_report(&mut out, report.as_ref())
            .unwrap();

        // Trim leading/trailing whitespace to avoid expect_test
        // indentation normalization issues.
        out.trim().to_string()
    }
}

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
    let parses = match T::parse(&scope, text) {
        Ok(v) => v,
        Err(errors) => {
            // Find the "best" error: the one that consumed the most input
            // (shortest remaining text). This heuristic picks the error from
            // the deepest successful parse attempt.
            let best_error = errors.iter().min_by_key(|e| e.text.len()).unwrap();

            let diagnostic = ParseDiagnostic::from_error(best_error, text);
            return Err(crate::anyhow!("{}", diagnostic.render()));
        }
    };

    // Filter to parses that consumed all input, keeping only the values.
    let mut values: Vec<T> = parses
        .into_iter()
        .filter(|p| skip_whitespace(p.text()).is_empty())
        .map(|p| p.finish().0)
        .collect();

    if values.is_empty() {
        crate::bail!("extra tokens after parsing {text:?}");
    }

    // Pop one value and remove all duplicates of it.
    let value = values.pop().unwrap();
    values.retain(|v| v != &value);

    if values.is_empty() {
        return Ok(value);
    }

    // Remaining values are genuinely different — ambiguous parse.
    values.push(value);

    let pretty: Vec<String> = values.iter().map(|v| format!("{v:#?}")).collect();

    let reference = &pretty[0];
    let mut msg = String::from("ambiguous parse of:\n\n");
    for line in text.lines() {
        msg.push_str(&format!("  {line}\n"));
    }
    msg.push_str(&format!("\ninterpretation 0 (reference):\n\n"));
    for line in reference.lines() {
        msg.push_str(&format!("  {line}\n"));
    }
    for (i, other) in pretty[1..].iter().enumerate() {
        msg.push_str(&format!(
            "\ninterpretation {} (diff from reference):\n\n",
            i + 1
        ));
        let diff = similar::TextDiff::from_lines(reference, other);
        for change in diff.iter_all_changes() {
            let sign = match change.tag() {
                similar::ChangeTag::Delete => "-",
                similar::ChangeTag::Insert => "+",
                similar::ChangeTag::Equal => " ",
            };
            msg.push_str(&format!("  {sign}{change}"));
        }
    }
    panic!("{msg}");
}

/// Traits required for parse results
pub trait ParseSuccessType: Sized + Debug + Clone + Ord + Eq + 'static + Upcast<Self> {}
impl<T: Debug + Clone + Ord + Eq + 'static + Upcast<T>> ParseSuccessType for T {}

/// Record from a successful parse.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SuccessfulParse<'t, T> {
    /// The new point in the input, after we've consumed whatever text we have.
    text: &'t str,

    /// The precedence of this parse, which is derived from the value given
    /// to `parse_variant`.
    precedence: Precedence,

    /// The value produced.
    value: T,
}

impl<'t, T> SuccessfulParse<'t, T> {
    /// Returns the remaining unparsed text.
    pub fn text(&self) -> &'t str {
        self.text
    }

    /// Extract the value parsed and the remaining text.
    pub fn finish(self) -> (T, &'t str) {
        (self.value, self.text)
    }

    /// Maps the value using `op` -- this is meant for 'no-op' conversions like going from `T` to `Arc<T>`.
    pub fn map<U>(self, op: impl FnOnce(T) -> U) -> SuccessfulParse<'t, U> {
        SuccessfulParse {
            text: self.text,
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
            precedence: term.precedence,
            value: term.value.upcast(),
        }
    }
}

/// A single frame in the nonterminal parse backtrace.
/// Records which nonterminal was being parsed and where
/// in the input it started.
#[derive(Clone, Debug)]
pub struct ParseFrame {
    /// Name of the nonterminal (e.g., "Expr", "Stmt", "FnBody").
    pub name: &'static str,

    /// Length of the remaining input when this nonterminal's parse began.
    /// Used to compute the byte offset: `full_input.len() - remaining_len`.
    pub remaining_len: usize,
}

/// Tracks an error that occurred while parsing.
/// The parse error records the input text it saw, which will be
/// some suffix of the original input, along with a message.
///
/// The actual [`ParseResult`] type tracks a *set* of parse errors.
/// When parse errors are generated, there is just one (e.g., "expected identifier"),
/// but when there are choice points in the grammar (e.g., when parsing an enum),
/// those errors can be combined by [`require_unambiguous`].
///
/// The `backtrace` field records the chain of nonterminals being parsed
/// when this error was created. It is excluded from `Ord`/`Eq` comparisons
/// so that errors at the same position with the same message are deduplicated
/// in `BTreeSet<ParseError>`.
#[derive(Clone, Debug)]
pub struct ParseError<'t> {
    /// Input that triggered the parse error. Some suffix
    /// of the original input.
    pub text: &'t str,

    /// Message describing what was expected.
    pub message: String,

    /// Chain of nonterminals being parsed when this error occurred,
    /// from outermost to innermost.
    pub backtrace: Vec<ParseFrame>,
}

impl<'t> PartialEq for ParseError<'t> {
    fn eq(&self, other: &Self) -> bool {
        self.text == other.text && self.message == other.message
    }
}

impl<'t> Eq for ParseError<'t> {}

impl<'t> PartialOrd for ParseError<'t> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'t> Ord for ParseError<'t> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (&self.text, &self.message).cmp(&(&other.text, &other.message))
    }
}

impl<'t> ParseError<'t> {
    /// Creates a single parse error at the given point. Returns
    /// a set so that it can be wrapped as a [`ParseResult`].
    ///
    /// Captures a snapshot of the current nonterminal parse stack
    /// as a backtrace for error reporting.
    pub fn at(text: &'t str, message: String) -> Set<Self> {
        let text = parser::skip_whitespace(text);

        let backtrace: Vec<ParseFrame> = parser::snapshot_nonterminal_stack();

        set![ParseError {
            text,
            message,
            backtrace,
        }]
    }

    pub fn expected_type<R>(text: &'t str) -> Set<Self> {
        Self::at(text, format!("{} expected", std::any::type_name::<R>()))
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
        consumed_any_since(text, self.text)
    }
}

pub type ParseResult<'t, T> = Result<Set<SuccessfulParse<'t, T>>, Set<ParseError<'t>>>;

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
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
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
            p.each_delimited_nonterminal('[', false, ']', |v, p| p.ok(v))
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
            p.each_delimited_nonterminal('{', false, '}', |v: Vec<T>, p| {
                p.ok(v.into_iter().collect())
            })
        })
    }
}

impl<L, T> CoreParse<L> for Option<T>
where
    L: Language,
    T: CoreParse<L>,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "Option", |p| {
            p.each_opt_nonterminal(|v, p| p.ok(v))
        })
    }
}

/// Binding grammar delegates to [`CoreParseBinding`] on `L::Parameter`.
impl<L: Language> CoreParse<L> for Binding<L> {
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        <L::Parameter as CoreParseBinding<L>>::parse_binding(scope, text)
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
                    return p.each_nonterminal(|data: T, p| p.ok(CoreBinder::dummy(data)));
                }
            }

            p.each_comma_nonterminal(|bindings: Vec<Binding<L>>, p| {
                p.expect_char(L::BINDING_CLOSE)?;

                let scope1 = scope.with_bindings(bindings.iter().map(|b| (&b.name, b.bound_var)));
                p.with_scope(scope1, |p| {
                    p.each_nonterminal(|data: T, p| {
                        let kvis: Vec<CoreBoundVar<L>> =
                            bindings.iter().map(|b| b.bound_var).collect();
                        p.ok(CoreBinder::new(kvis, data))
                    })
                })
            })
        })
    }
}

impl<L, T> CoreParse<L> for Arc<T>
where
    L: Language,
    T: CoreParse<L>,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        T::parse(scope, text)
            .map(|successes| successes.into_iter().map(|s| s.map(Arc::new)).collect())
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
                p.ok(true)
            });

            parser.parse_variant("false", Precedence::default(), |p| {
                p.expect_keyword("false")?;
                p.ok(false)
            });
        })
    }
}

impl<L> CoreParse<L> for usize
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "usize", |p| {
            let v = p.number()?;
            p.ok(v)
        })
    }
}

impl<L> CoreParse<L> for u8
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "u8", |p| {
            let v = p.number()?;
            p.ok(v)
        })
    }
}

impl<L> CoreParse<L> for u16
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "u16", |p| {
            let v = p.number()?;
            p.ok(v)
        })
    }
}

impl<L> CoreParse<L> for u32
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "u32", |p| {
            let v = p.number()?;
            p.ok(v)
        })
    }
}

impl<L> CoreParse<L> for u64
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "u64", |p| {
            let v = p.number()?;
            p.ok(v)
        })
    }
}

impl<L> CoreParse<L> for i8
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "u16", |p| {
            let v = p.number()?;
            p.ok(v)
        })
    }
}

impl<L> CoreParse<L> for i16
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "u16", |p| {
            let v = p.number()?;
            p.ok(v)
        })
    }
}

impl<L> CoreParse<L> for i32
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "u16", |p| {
            let v = p.number()?;
            p.ok(v)
        })
    }
}

impl<L> CoreParse<L> for i64
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "u16", |p| {
            let v = p.number()?;
            p.ok(v)
        })
    }
}

impl<L> CoreParse<L> for isize
where
    L: Language,
{
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "u16", |p| {
            let v = p.number()?;
            p.ok(v)
        })
    }
}

impl<L: Language> CoreParse<L> for () {
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "`()`", |p| {
            p.expect_char('(')?;
            p.expect_char(')')?;
            p.ok(())
        })
    }
}

impl<L: Language, A: CoreParse<L>, B: CoreParse<L>> CoreParse<L> for (A, B) {
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "tuple", |p| {
            p.expect_char('(')?;
            p.each_nonterminal(|a: A, p| {
                p.expect_char(',')?;
                p.each_nonterminal(|b: B, p| {
                    p.skip_trailing_comma();
                    p.expect_char(')')?;
                    p.ok((a.clone(), b))
                })
            })
        })
    }
}

impl<L: Language, A: CoreParse<L>, B: CoreParse<L>, C: CoreParse<L>> CoreParse<L> for (A, B, C) {
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "tuple", |p| {
            p.expect_char('(')?;
            p.each_nonterminal(|a: A, p| {
                p.expect_char(',')?;
                p.each_nonterminal(|b: B, p| {
                    p.expect_char(',')?;
                    p.each_nonterminal(|c: C, p| {
                        p.skip_trailing_comma();
                        p.expect_char(')')?;
                        p.ok((a.clone(), b.clone(), c))
                    })
                })
            })
        })
    }
}

impl<L: Language> CoreParse<L> for CoreVariable<L> {
    fn parse<'t>(scope: &Scope<L>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "variable", |p| {
            let v = p.variable()?;
            p.ok(v)
        })
    }
}
