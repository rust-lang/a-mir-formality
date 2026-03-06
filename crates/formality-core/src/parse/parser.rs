use std::fmt::Debug;
use std::str::FromStr;

use crate::{
    language::Language,
    parse::parser::left_recursion::{CurrentState, LeftRight},
    set,
    variable::CoreVariable,
    Set, Upcast,
};

use super::{CoreParse, ParseError, ParseResult, Scope, SuccessfulParse, TokenResult};

mod left_recursion;

/// Create this struct when implementing the [`CoreParse`][] trait.
/// Each `Parser` corresponds to some symbol in the grammar.
/// You create a parser and then you invoke the `parse_variant`
/// (or `parse_variant_cast`) method for each of the distinct possible parsings
/// for that symbol. Finally you invoke `finish` to complete the parse.
/// So e.g. if you grammar contains `Foo = Bar | Baz`, then you would
///
/// * create a `Parser` in the [`CoreParse`][] impl for `Foo`
/// * invoke `parse_variant` twice, once for `Bar` and once for `Baz`
/// * invoke `finish`
pub struct Parser<'s, 't, T, L>
where
    L: Language,
    T: Debug + Clone + Eq + 'static,
{
    scope: &'s Scope<L>,
    start_text: &'t str,
    nonterminal_name: &'static str,
    successes: Vec<SuccessfulParse<'t, T>>,
    failures: Set<ParseError<'t>>,
    min_precedence_level: usize,
}

/// The *precedence* of a variant determines how to manage
/// recursive invocations.
///
/// The general rule is that an expression
/// with lower-precedence cannot be embedded
/// into an expression of higher-precedence.
/// So given `1 + 2 * 3`, the `+` cannot be a
/// (direct) child of the `*`, because `+` is
/// lower precedence.
///
/// The tricky bit is what happens with *equal*
/// precedence. In that case, we have to consider
/// the [`Associativity`][] (see enum for details).
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Precedence {
    level: usize,
    associativity: Associativity,
}

/// Determines what happens when you have equal precedence.
/// The result is dependent on whether you are embedding
/// in left position (i.e., a recurrence before having parsed any
/// tokens) or right position (a recurrence after parsed tokens).
/// So given `1 + 2 + 3`, `1 + 2` is a *left* occurrence of the second `+`.
/// And `2 + 3` is a *right* occurence of the first `+`.
///
/// With `Associativity::Left`, equal precedence is allowed in left matches
/// but not right. So `1 + 2 + 3` parses as `(1 + 2) + 3`, as you would expect.
///
/// With `Associativity::Right`, equal precedence is allowed in right matches
/// but not left. So `1 + 2 + 3` parses as `1 + (2 + 3)`. That's probably not what you wanted
/// for arithemetic expressions, but could be useful for (say) curried function types,
/// where `1 -> 2 -> 3` should parse as `1 -> (2 -> 3)`.
///
/// With `Associativity::None`, equal precedence is not allowed anywhere, so
/// `1 + 2 + 3` is just an error and you have to explicitly add parentheses.
///
/// Use `Precedence::default` for cases where precedence is not relevant.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Associativity {
    Left,
    Right,
    None,
    Both,
}

impl Precedence {
    /// Left associative with the given precedence level
    pub fn left(level: usize) -> Self {
        Self::new(level, Associativity::Left)
    }

    /// Right associative with the given precedence level
    pub fn right(level: usize) -> Self {
        Self::new(level, Associativity::Right)
    }

    /// Non-associative with the given precedence level
    pub fn none(level: usize) -> Self {
        Self::new(level, Associativity::None)
    }

    /// Construct a new precedence.
    fn new(level: usize, associativity: Associativity) -> Self {
        // We require level to be STRICTLY LESS than usize::MAX
        // so that we can always add 1.
        assert!(level < std::usize::MAX);
        Self {
            level,
            associativity,
        }
    }
}

impl Default for Precedence {
    fn default() -> Self {
        // Default precedence:
        //
        // Use MAX-1 because we sometimes try to add 1 when we detect recursion, and we don't want overflow.
        //
        // Use Right associativity because if you have a variant like `T = [T]`
        // then you want to be able to (by default) embed arbitrary T in that recursive location.
        // If you use LEFT or NONE, then this recursive T would have a minimum level of std::usize::MAX
        // (and hence never be satisfied).
        //
        // Using RIGHT feels a bit weird here but seems to behave correctly all the time.
        // It's tempting to add something like "Both" or "N/A" that would just not set a min
        // prec level when there's recursion.
        Self::new(std::usize::MAX - 1, Associativity::Both)
    }
}

/// The "active variant" struct is the main struct
/// you will use if you are writing custom parsing logic.
/// It contains various methods for consuming tokens
/// (e.g., `identifier`, `number`, `variable`).
///
/// The most common method is `each_nonterminal`,
/// which lets you parse an instance of some other
/// type that implements [`CoreParse`][] and run a
/// continuation for each successful parse.
///
/// Another common set of methods are things like
/// `each_comma_nonterminal`, which parses a comma-separated
/// list of nonterminals and passes the result to a continuation.
///
/// Note that **all** the methods of active variant skip past whitespace (and comments) implicitly
#[derive(Clone, Debug)]
pub struct ActiveVariant<'s, 't, L>
where
    L: Language,
{
    precedence: Precedence,
    scope: &'s Scope<L>,
    start_text: &'t str,
    current_text: &'t str,

    /// A variant is 'committed' when we have seen enough success
    is_committed: bool,
}

impl<'s, 't, T, L> Parser<'s, 't, T, L>
where
    L: Language,
    T: Debug + Clone + Eq + 'static + Upcast<T>,
{
    /// Shorthand to create a parser for a nonterminal with a single variant,
    /// parsed by the function `op`.
    ///
    /// The nonterminal-name is used as the variant name.
    pub fn single_variant(
        scope: &'s Scope<L>,
        text: &'t str,
        nonterminal_name: &'static str,
        mut op: impl FnMut(&mut ActiveVariant<'s, 't, L>) -> ParseResult<'t, T>,
    ) -> ParseResult<'t, T> {
        Parser::multi_variant(scope, text, nonterminal_name, |parser| {
            parser.parse_variant(nonterminal_name, Precedence::default(), &mut op);
        })
    }

    /// Creates a new parser that can accommodate multiple variants.
    ///
    /// Invokes `op` to do the parsing; `op` should call `parse_variant` 0 or more times for
    /// each of the possibilities. The best result (if any) will then be returned.
    ///
    /// The method [`single_variant`] is more convenient if you have exactly one variant.
    pub fn multi_variant(
        scope: &'s Scope<L>,
        text: &'t str,
        nonterminal_name: &'static str,
        mut op: impl FnMut(&mut Self),
    ) -> ParseResult<'t, T> {
        let text = skip_whitespace(text);

        left_recursion::enter(scope, text, |min_precedence_level| {
            let tracing_span = tracing::span!(
                tracing::Level::TRACE,
                "nonterminal",
                name = nonterminal_name,
                ?scope,
                ?text
            );
            let guard = tracing_span.enter();

            let mut parser = Self {
                scope,
                start_text: text,
                nonterminal_name,
                successes: vec![],
                failures: set![],
                min_precedence_level,
            };

            op(&mut parser);

            parser.finish(guard)
        })
    }

    /// Parse an identifier as a standalone nonterminal.
    /// Used by the `id!` macro.
    pub fn identifier_nonterminal(
        scope: &'s Scope<L>,
        text: &'t str,
        nonterminal_name: &'static str,
    ) -> ParseResult<'t, T>
    where
        String: Into<T>,
    {
        Self::single_variant(scope, text, nonterminal_name, |p| {
            let id = p.identifier()?;
            p.ok(id.into())
        })
    }

    /// Like [`Self::identifier_nonterminal`], but rejects identifiers that match a variable in scope.
    pub fn identifier_no_var(
        scope: &'s Scope<L>,
        text: &'t str,
        nonterminal_name: &'static str,
    ) -> ParseResult<'t, T>
    where
        String: Into<T>,
    {
        Self::single_variant(scope, text, nonterminal_name, |p| {
            p.reject_variable()?;
            let id = p.identifier()?;
            p.ok(id.into())
        })
    }

    /// Like [`Self::identifier_nonterminal`], but uses a custom regex to match the identifier string.
    /// The regex should already be anchored at the start (`^`).
    /// Still rejects language keywords.
    pub fn identifier_re(
        scope: &'s Scope<L>,
        text: &'t str,
        nonterminal_name: &'static str,
        re: &regex::Regex,
    ) -> ParseResult<'t, T>
    where
        String: Into<T>,
    {
        Self::single_variant(scope, text, nonterminal_name, |p| {
            let s = p.regex_str(re, nonterminal_name)?;
            p.reject_custom_keywords(L::KEYWORDS)?;
            p.ok(s.into())
        })
    }

    /// Shorthand for `parse_variant` where the parsing operation is to
    /// parse the type `V` and then upcast it to the desired result type.
    pub fn parse_variant_cast<V>(&mut self, variant_precedence: Precedence)
    where
        V: CoreParse<L> + Upcast<T>,
    {
        let variant_name = std::any::type_name::<V>();
        Self::parse_variant(self, variant_name, variant_precedence, |p| {
            p.each_nonterminal(|v: V, p| p.ok(v))
        })
    }

    pub fn parse_variant_variable(
        &mut self,
        variant_name: &'static str,
        variant_precedence: Precedence,
        kind: L::Kind,
    ) where
        CoreVariable<L>: Upcast<T>,
    {
        Self::parse_variant(self, variant_name, variant_precedence, |p| {
            let v = p.variable_of_kind(kind)?;
            p.ok(v)
        })
    }

    /// Returns true if any variants have already parsed successfully.
    /// This is used by generated code to short-circuit: if a `#[variable]`
    /// variant matched, we skip all other variants.
    pub fn has_successes(&self) -> bool {
        !self.successes.is_empty()
    }

    /// Parses a single variant for this nonterminal.
    /// The precedence is part of how we resolve conflicts: if there are two successful parses with distinct precedence, higher precedence wins.
    /// The `op` is a closure that defines how the variant itself is parsed.
    /// The closure `op` will be invoked with an [`ActiveVariant`][] struct that has methods for consuming identifiers, numbers, keywords, etc.
    ///
    /// The closure returns a `ParseResult` — it may produce multiple successful parses
    /// (e.g., when child nonterminals are ambiguous and ambiguity propagates via `each_nonterminal`).
    pub fn parse_variant<U>(
        &mut self,
        variant_name: &'static str,
        variant_precedence: Precedence,
        op: impl FnOnce(&mut ActiveVariant<'s, 't, L>) -> ParseResult<'t, U>,
    ) where
        U: Upcast<T>,
    {
        let span = tracing::span!(
            tracing::Level::TRACE,
            "variant",
            name = variant_name,
            ?variant_precedence,
        );
        let guard = span.enter();

        if variant_precedence.level < self.min_precedence_level {
            tracing::trace!(
                "variant has precedence level {} which is below parser minimum of {}",
                variant_precedence.level,
                self.min_precedence_level,
            );
            return;
        }

        let mut active_variant =
            ActiveVariant::new(variant_precedence, self.scope, self.start_text);
        let result = op(&mut active_variant);

        // Drop the guard here so that the "success" or "error" results appear outside the variant span.
        // This makes it easier to follow along with parsing when you collapse sections.
        drop(guard);

        match result {
            Ok(successes) => {
                for success in successes {
                    let success = SuccessfulParse {
                        text: success.text,
                        precedence: variant_precedence,
                        value: success.value.upcast(),
                    };

                    tracing::trace!("success: {:?}", success);
                    self.successes.push(success);
                }
            }

            Err(errs) => {
                // If this variant encountered failures, then record them -- but careful!
                // We only record failures where we actually consumed any tokens.
                // This is part of our error reporting and recovery mechanism.
                // Note that we expect (loosely) an LL(1) grammar.
                if active_variant.is_committed {
                    self.failures.extend(
                        errs.into_iter()
                            .filter(|e| e.consumed_any_since(self.start_text))
                            .inspect(|e| tracing::trace!("error: {e:?}")),
                    );
                }
            }
        }
    }

    /// Returns true if at least one variant has been successfully parsed.
    /// Used by generated code to short-circuit after a `#[variable]` variant
    /// succeeds, skipping remaining variants.
    pub fn has_success(&self) -> bool {
        !self.successes.is_empty()
    }

    fn finish(self, guard: tracing::span::Entered<'_>) -> ParseResult<'t, T> {
        // If we did not parse anything successfully, then return an error.
        // There are two possibilities: some of our variants may have made
        // progress but ultimately failed. If so, self.failures will be non-empty,
        // and we can return that. Otherwise, none of our variants made any
        // progress at all, so we create a failure at the starting point
        // just saying "we failed to find a X here". If this nonterminal is at
        // the start of some larger nonterminal, that larger nonterminal will
        // observe that we did not consume any tokens and will ignore our messawge
        // and put its own (e.g., "failed to find a Y here").
        if self.successes.is_empty() {
            // It's better to print this result alongside the main parsing section.
            drop(guard);
            return if self.failures.is_empty() {
                tracing::trace!("parsing failed: no variants were able to consume a single token");
                Err(ParseError::at(
                    self.start_text,
                    format!("{} expected", self.nonterminal_name),
                ))
            } else {
                tracing::trace!(
                    "parsing failed with {} errors `{:?}`",
                    self.failures.len(),
                    self.failures
                );
                Err(self.failures)
            };
        }

        if self.successes.len() > 1 {
            tracing::trace!("successful parses: `{:?}`", self.successes);
        }

        // Return all successful parses. Disambiguation happens at the top level
        // (in core_term_with) rather than here, so ambiguous parses propagate
        // upward instead of panicking.
        drop(guard);
        Ok(self.successes)
    }
}

impl<'s, 't, L> ActiveVariant<'s, 't, L>
where
    L: Language,
{
    fn new(precedence: Precedence, scope: &'s Scope<L>, start_text: &'t str) -> Self {
        let start_text = skip_whitespace(start_text);
        Self {
            precedence,
            scope,
            start_text,
            current_text: start_text,
            is_committed: true,
        }
    }

    /// A variant is "committed" when it has parsed enough tokens
    /// for us to be reasonably sure this is what the user meant to type.
    /// At that point, any parse errors will propagate out.
    /// This is important for optional or repeated nonterminals.
    ///
    /// By default, variants start with committed set to true.
    /// You can clear it to false explicitly and set it back to true later
    /// once you've seen enough parsing.
    ///
    /// Regardless of the value of this flag, any error that occurs before
    /// we have consumed any tokens at all will be considered uncommitted.
    ///
    /// With auto-generated parsers, this flag is used to implement the `$!`
    /// marker. If that marker is present, we set committed to false initially,
    /// and then set it to true when we encounter a `$!`.
    pub fn set_committed(&mut self, value: bool) {
        tracing::trace!("set_committed({})", value);
        self.is_committed = value;
    }

    fn current_state(&self) -> CurrentState {
        // Determine whether we are in Left or Right position -- Left means
        // that we have not yet consumed any tokens. Right means that we have.
        // See `LeftRight` type for more details.
        //
        // Subtle-ish: this comparison assumes there is no whitespace,
        // but we establish that invariant in `Self::new`.
        debug_assert_eq!(self.start_text, skip_whitespace(self.start_text));
        let left_right = if self.start_text == self.current_text {
            LeftRight::Left
        } else {
            LeftRight::Right
        };

        CurrentState {
            left_right,
            precedence: self.precedence,
            current_text: self.current_text,
        }
    }

    /// The current text remaining to be consumed.
    pub fn text(&self) -> &'t str {
        self.current_text
    }

    /// Return the set of variables in scope
    pub fn scope(&self) -> &Scope<L> {
        self.scope
    }

    /// Skips whitespace in the input.
    pub fn skip_whitespace(&mut self) {
        self.current_text = skip_whitespace(self.current_text);
    }

    /// Skips a comma in the input.
    pub fn skip_trailing_comma(&mut self) {
        self.current_text = skip_trailing_comma(self.current_text);
    }

    /// Expect *exactly* the given text (after skipping whitespace)
    /// in the input string. Reports an error if anything else is observed.
    /// In error case, consumes only whitespace.
    pub fn expect_char(&mut self, char: char) -> Result<(), Set<ParseError<'t>>> {
        self.token(|text| {
            if text.starts_with(char) {
                Ok(((), &text[char.len_utf8()..]))
            } else {
                Err(ParseError::at(text, format!("expected `{}`", char)))
            }
        })
    }

    /// Extracts a series of characters from the text
    /// (after skipping whitespace).
    /// The first character must match `start_test`
    /// and all subsequent characters must match `continue_test`.
    pub fn string(
        &mut self,
        start_test: impl Fn(char) -> bool,
        continue_test: impl Fn(char) -> bool,
        description: &'static str,
    ) -> Result<String, Set<ParseError<'t>>> {
        self.token(|text0| {
            let mut buffer = String::new();

            let (ch, text) = next_char(text0)?;
            if !start_test(ch) {
                return Err(ParseError::at(text0, format!("{} expected", description)));
            }
            buffer.push(ch);

            let mut text = text;
            while let Ok((ch, t)) = next_char(text) {
                if !continue_test(ch) {
                    break;
                }

                buffer.push(ch);
                text = t;
            }

            Ok((buffer, text))
        })
    }

    /// Consume next identifier-like string, requiring that it be equal to `expected`.
    #[tracing::instrument(level = "trace", ret)]
    pub fn expect_keyword(&mut self, expected: &str) -> Result<(), Set<ParseError<'t>>> {
        let text0 = self.current_text;
        match self.identifier_like_string() {
            Ok(ident) if &*ident == expected => Ok(()),
            _ => Err(ParseError::at(
                skip_whitespace(text0),
                format!("expected `{}`", expected),
            )),
        }
    }

    /// Accepts any of the given keywords.
    #[tracing::instrument(level = "trace", skip(self), ret)]
    pub fn expect_keyword_in(&mut self, expected: &[&str]) -> Result<String, Set<ParseError<'t>>> {
        let text0 = self.current_text;
        match self.identifier_like_string() {
            Ok(ident) if expected.iter().any(|&kw| ident == kw) => Ok(ident),
            _ => Err(ParseError::at(
                skip_whitespace(text0),
                format!("expected any of `{:?}`", expected),
            )),
        }
    }

    /// Attempts to execute `op` and, if it successfully parses, then returns an error
    /// with the value (which is meant to be incorporated into a par)
    /// If `op` fails to parse then the result is `Ok`.
    #[tracing::instrument(level = "trace", skip(self, op, err), ret)]
    pub fn reject<T>(
        &self,
        op: impl Fn(&mut ActiveVariant<'s, 't, L>) -> Result<T, Set<ParseError<'t>>>,
        err: impl FnOnce(T) -> Set<ParseError<'t>>,
    ) -> Result<(), Set<ParseError<'t>>> {
        let mut this = ActiveVariant {
            precedence: self.precedence,
            start_text: self.start_text,
            current_text: self.current_text,
            scope: self.scope,
            is_committed: true,
        };

        match op(&mut this) {
            Ok(value) => Err(err(value)),
            Err(_) => Ok(()),
        }
    }

    /// Reject the next identifier if it matches a variable currently in scope.
    /// Does not consume any input. Used by `id!` types with `match_var = false`
    /// to avoid ambiguity between variables and identifiers.
    pub fn reject_variable(&self) -> Result<(), Set<ParseError<'t>>> {
        self.reject(
            |p| p.variable(),
            |var| {
                ParseError::at(
                    self.current_text,
                    format!("expected identifier, found variable `{var:?}` in scope"),
                )
            },
        )
    }

    /// Reject next identifier-like string if it is one of the given list of keywords.
    /// Does not consume any input.
    /// You can this to implement positional keywords -- just before parsing an identifier or variable,
    /// you can invoke `reject_custom_keywords` to reject anything that you don't want to permit in this position.
    pub fn reject_custom_keywords(&self, keywords: &[&str]) -> Result<(), Set<ParseError<'t>>> {
        self.reject(
            |p| p.expect_keyword_in(keywords),
            |ident| {
                ParseError::at(
                    self.current_text,
                    format!("expected identified, found keyword `{ident:?}`"),
                )
            },
        )
    }

    /// Extracts a string that meets the regex for an identifier
    /// (but it could also be a keyword).
    #[tracing::instrument(level = "trace", skip(self), ret)]
    pub fn identifier_like_string(&mut self) -> Result<String, Set<ParseError<'t>>> {
        self.string(
            |ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '_'),
            |ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'),
            "identifier",
        )
    }

    /// Extracts a string matching the given regex from the start of text.
    /// The regex should be anchored at the start (caller is responsible for `^`).
    pub fn regex_str(
        &mut self,
        re: &regex::Regex,
        description: &'static str,
    ) -> Result<String, Set<ParseError<'t>>> {
        self.token(|text0| match re.find(text0) {
            Some(m) if m.start() == 0 && !m.is_empty() => {
                Ok((m.as_str().to_string(), &text0[m.end()..]))
            }
            _ => Err(ParseError::at(text0, format!("{} expected", description))),
        })
    }

    /// Extracts a maximal identifier from the start of text,
    /// following the usual rules. **Disallows language keywords.**
    /// If you want to disallow additional keywords,
    /// see the `reject_custom_keywords` method.
    #[tracing::instrument(level = "trace", skip(self), ret)]
    pub fn identifier(&mut self) -> Result<String, Set<ParseError<'t>>> {
        self.reject_custom_keywords(L::KEYWORDS)?;
        self.identifier_like_string()
    }

    /// Change the set of bindings in scope.
    /// Invokes `op` with a new active variant.
    pub fn with_scope<R>(
        &mut self,
        scope: Scope<L>,
        op: impl FnOnce(&mut ActiveVariant<'_, 't, L>) -> R,
    ) -> R {
        let mut av = ActiveVariant {
            precedence: self.precedence,
            scope: &scope,
            start_text: self.start_text,
            current_text: self.current_text,
            is_committed: true,
        };
        let result = op(&mut av);
        self.current_text = av.current_text;
        result
    }

    /// Parses the next identifier as a variable in scope.
    ///
    /// **NB:** This departs from the limits of context-free
    /// grammars -- the scope is a piece of context that affects how
    /// our parsing proceeds!
    #[tracing::instrument(level = "trace", skip(self), ret)]
    pub fn variable(&mut self) -> Result<CoreVariable<L>, Set<ParseError<'t>>> {
        self.skip_whitespace();
        let text0 = self.current_text;
        let id = <L::Parameter as super::CoreParseBinding<L>>::parse_variable_name(self)?;
        match self.scope.lookup(&id) {
            Some(v) => Ok(v),
            None => Err(ParseError::at(text0, "unrecognized variable".to_string())),
        }
    }

    /// Parses the next identifier as a [variable in scope](`Self::variable`)
    /// and checks that it has the right kind. This is a useful
    /// helper method to avoid mis-kinding variables.
    #[tracing::instrument(level = "trace", skip(self), ret)]
    pub fn variable_of_kind(
        &mut self,
        kind: L::Kind,
    ) -> Result<CoreVariable<L>, Set<ParseError<'t>>> {
        self.skip_whitespace();
        let text0 = self.current_text;
        let v = self.variable()?;
        if v.kind() == kind {
            Ok(v)
        } else {
            Err(ParseError::at(
                text0,
                format!(
                    "expected a variable of kind `{:?}`, found `{:?}`, which has kind `{:?}`",
                    kind,
                    v,
                    v.kind()
                ),
            ))
        }
    }

    /// Extract a number from the input, erroring if the input does not start with a number.
    #[tracing::instrument(level = "trace", skip(self), ret)]
    pub fn number<T>(&mut self) -> Result<T, Set<ParseError<'t>>>
    where
        T: FromStr + std::fmt::Debug,
    {
        let description = std::any::type_name::<T>();
        let text0 = self.current_text;
        let s = self.string(char::is_numeric, char::is_numeric, description)?;
        match T::from_str(&s) {
            Ok(t) => Ok(t),
            Err(_) => Err(ParseError::at(
                skip_whitespace(text0),
                "invalid number".to_string(),
            )),
        }
    }

    /// Consumes a single token from the input after skipping whitespace.
    ///
    /// The token is defined by `op`, which is a function that takes
    /// the text and returns a result + new text.
    fn token<T>(
        &mut self,
        op: impl FnOnce(&'t str) -> TokenResult<'t, T>,
    ) -> Result<T, Set<ParseError<'t>>> {
        self.skip_whitespace();
        let value;
        (value, self.current_text) = op(self.current_text)?;
        Ok(value)
    }

    /// Gives an error if `T` is parsable here.
    pub fn reject_nonterminal<T>(&self) -> Result<(), Set<ParseError<'t>>>
    where
        T: CoreParse<L>,
    {
        self.reject(
            |p| {
                // We only need to know if *any* parse succeeds, so just check the result.
                let successes = T::parse(p.scope, p.current_text)?;
                let success = successes.into_iter().next().unwrap();
                p.current_text = success.text;
                Ok(success.value)
            },
            |value| ParseError::at(self.text(), format!("unexpected `{value:?}`")),
        )
    }

    /// Try to parse the current point as `T` and run the continuation for each
    /// successful parse with `Some(t)`. If no parse succeeds (without consuming
    /// tokens), runs the continuation with `None`.
    ///
    /// **NB:**  If the parse partially succeeds, i.e., we are able to consume some tokens
    /// but cannot completely parse, then this is an error, not a `None` result.
    /// So for example if the grammar for `T` is `()` and the input is `"foo"`,
    /// then we will get `None`. But if the input were `"(foo)"`, we would get an error,
    /// because we consumed the open paren `(` from `T` but then encountered an error
    /// looking for the closing paren `)`.
    #[track_caller]
    #[tracing::instrument(level = "Trace", skip(self, op), ret)]
    pub fn each_opt_nonterminal<T, R>(
        &mut self,
        op: impl Fn(Option<T>, &mut ActiveVariant<'s, 't, L>) -> ParseResult<'t, R>,
    ) -> ParseResult<'t, R>
    where
        T: CoreParse<L>,
        R: Debug + Clone + Eq + 'static,
    {
        let text0 = self.current_text;
        match self.each_nonterminal(|t: T, p| op(Some(t), p)) {
            Ok(successes) => Ok(successes),
            Err(mut errs) => {
                errs.retain(|e| e.consumed_any_since(text0));
                if errs.is_empty() {
                    // Nothing consumed — parse as None
                    assert_eq!(skip_whitespace(text0), self.current_text);
                    tracing::trace!(
                        "each_opt_nonterminal({}): parsing did not consume tokens or did not commit",
                        std::any::type_name::<T>()
                    );
                    op(None, self)
                } else {
                    tracing::trace!(
                        "each_opt_nonterminal({}): 'almost' succeeded with parse",
                        std::any::type_name::<T>()
                    );
                    Err(errs)
                }
            }
        }
    }

    /// Continue parsing instances of `T` while we can, then run
    /// the continuation with each possible sequence result.
    /// This is a greedy parse that propagates ambiguity.
    #[tracing::instrument(level = "Trace", skip(self, op), ret)]
    pub fn each_many_nonterminal<T, R>(
        &mut self,
        op: impl Fn(Vec<T>, &mut ActiveVariant<'s, 't, L>) -> ParseResult<'t, R>,
    ) -> ParseResult<'t, R>
    where
        T: CoreParse<L> + Clone,
        R: Debug + Clone + Eq + 'static,
    {
        self.each_many_nonterminal_accum(vec![], &op)
    }

    fn each_many_nonterminal_accum<T, R>(
        &mut self,
        acc: Vec<T>,
        op: &impl Fn(Vec<T>, &mut ActiveVariant<'s, 't, L>) -> ParseResult<'t, R>,
    ) -> ParseResult<'t, R>
    where
        T: CoreParse<L> + Clone,
        R: Debug + Clone + Eq + 'static,
    {
        self.each_opt_nonterminal(|opt_t: Option<T>, p| match opt_t {
            Some(t) => {
                let mut new_acc = acc.clone();
                new_acc.push(t);
                p.each_many_nonterminal_accum(new_acc, op)
            }
            None => op(acc.clone(), p),
        })
    }

    #[tracing::instrument(level = "Trace", skip(self, op), ret)]
    pub fn each_delimited_nonterminal<T, R>(
        &mut self,
        open: char,
        optional: bool,
        close: char,
        op: impl Fn(Vec<T>, &mut ActiveVariant<'s, 't, L>) -> ParseResult<'t, R>,
    ) -> ParseResult<'t, R>
    where
        T: CoreParse<L> + Clone,
        R: Debug + Clone + Eq + 'static,
    {
        // Look for the opening delimiter.
        // If we don't find it, then this is either an empty vector (if optional) or an error (otherwise).
        match self.expect_char(open) {
            Ok(()) => {}
            Err(errs) => {
                return if optional {
                    op(vec![], self)
                } else {
                    Err(errs)
                };
            }
        }

        // Now parse the contents.
        self.each_comma_nonterminal(|items: Vec<T>, p| {
            p.expect_char(close)?;
            op(items, p)
        })
    }

    /// Parse multiple instances of `T` separated by commas, then run
    /// the continuation with each possible sequence result.
    #[track_caller]
    pub fn each_comma_nonterminal<T, R>(
        &mut self,
        op: impl Fn(Vec<T>, &mut ActiveVariant<'s, 't, L>) -> ParseResult<'t, R>,
    ) -> ParseResult<'t, R>
    where
        T: CoreParse<L> + Clone,
        R: Debug + Clone + Eq + 'static,
    {
        self.each_comma_nonterminal_accum(vec![], &op)
    }

    fn each_comma_nonterminal_accum<T, R>(
        &mut self,
        acc: Vec<T>,
        op: &impl Fn(Vec<T>, &mut ActiveVariant<'s, 't, L>) -> ParseResult<'t, R>,
    ) -> ParseResult<'t, R>
    where
        T: CoreParse<L> + Clone,
        R: Debug + Clone + Eq + 'static,
    {
        self.each_opt_nonterminal(|opt_t: Option<T>, p| match opt_t {
            Some(t) => {
                let mut new_acc = acc.clone();
                new_acc.push(t);
                if p.expect_char(',').is_ok() {
                    p.each_comma_nonterminal_accum(new_acc, op)
                } else {
                    op(new_acc, p)
                }
            }
            None => op(acc.clone(), p),
        })
    }

    /// Parse a nonterminal and run a continuation for each successful parse,
    /// propagating ambiguity. This is the primary way to parse nonterminals
    /// in variant closures.
    ///
    /// For each successful parse of `T`, the continuation `op` is called
    /// with the parsed value and a forked `ActiveVariant` positioned at
    /// that parse's text position.
    /// All results from all continuations are collected and returned.
    #[track_caller]
    pub fn each_nonterminal<T, R>(
        &mut self,
        op: impl Fn(T, &mut ActiveVariant<'s, 't, L>) -> ParseResult<'t, R>,
    ) -> ParseResult<'t, R>
    where
        T: CoreParse<L>,
        R: Debug + Clone + Eq + 'static,
    {
        self.each_nonterminal_with(T::parse, op)
    }

    /// Like `each_nonterminal` but with a custom parse function.
    #[track_caller]
    pub fn each_nonterminal_with<T, R>(
        &mut self,
        parse_op: impl FnOnce(&'s Scope<L>, &'t str) -> ParseResult<'t, T>,
        cont: impl Fn(T, &mut ActiveVariant<'s, 't, L>) -> ParseResult<'t, R>,
    ) -> ParseResult<'t, R>
    where
        T: Debug + Clone + Eq + 'static,
        R: Debug + Clone + Eq + 'static,
    {
        self.skip_whitespace();

        let successes = left_recursion::recurse(self.current_state(), || {
            parse_op(self.scope, self.current_text)
        })?;

        let mut all_successes: Vec<SuccessfulParse<'t, R>> = vec![];
        let mut all_errors: Set<ParseError<'t>> = set![];

        for success in successes {
            // Fork the active variant for this parse result
            let mut forked = self.clone();
            forked.current_text = success.text;

            // Run the continuation
            match cont(success.value, &mut forked) {
                Ok(cont_successes) => {
                    all_successes.extend(cont_successes);
                }
                Err(errs) => {
                    all_errors.extend(errs);
                }
            }
        }

        if all_successes.is_empty() {
            if all_errors.is_empty() {
                // No child parses succeeded at all — this shouldn't normally happen
                // since we already got successes from the child, but the continuations
                // all failed. Return the continuation errors.
                Err(ParseError::at(
                    self.current_text,
                    format!(
                        "{} expected (continuation failed)",
                        std::any::type_name::<R>()
                    ),
                ))
            } else {
                Err(all_errors)
            }
        } else {
            Ok(all_successes)
        }
    }

    /// Wrap a single value into a `ParseResult` at the current parsing position.
    /// This is the leaf of an `each_nonterminal` chain — it creates a single
    /// `SuccessfulParse` capturing the current text position.
    pub fn ok<T: Debug + Clone + Eq + 'static>(&self, value: T) -> ParseResult<'t, T> {
        Ok(vec![SuccessfulParse {
            text: self.current_text,
            precedence: self.precedence,
            value,
        }])
    }

}

/// Extract the next character from input, returning an error if we've reached the input.
///
/// Warning: does not skip whitespace.
pub fn next_char(text: &str) -> TokenResult<'_, char> {
    let ch = match text.chars().next() {
        Some(c) => c,
        None => return Err(ParseError::at(text, "unexpected end of input".to_string())),
    };
    Ok((ch, &text[char::len_utf8(ch)..]))
}

/// Consume a comma if one is present.
#[tracing::instrument(level = "trace", ret)]
pub fn skip_trailing_comma(text: &str) -> &str {
    text.strip_prefix(',').unwrap_or(text)
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
