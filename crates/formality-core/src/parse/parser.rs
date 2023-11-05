use std::fmt::Debug;
use std::str::FromStr;

use crate::{
    language::{CoreParameter, HasKind, Language},
    set, Downcast, DowncastFrom, Set, Upcast,
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
    successes: Vec<(SuccessfulParse<'t, T>, Precedence)>,
    failures: Set<ParseError<'t>>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Precedence(usize);

/// The "active variant" struct is the main struct
/// you will use if you are writing custom parsing logic.
/// It contains various methods for consuming tokens
/// (e.g., `identifier`, `number`, `variable`).
///
/// The most common method is `nonterminal`,
/// which lets you parse an instance of some other
/// type that implements [`CoreParse`][].
///
/// Another common set of methods are things like
/// `comma_nonterminal`, which parses a comma-separated
/// list of nonterminals.
///
/// Note that **all** the methods of active variant skip past whitespace (and comments) implicitly
#[derive(Clone, Debug)]
pub struct ActiveVariant<'s, 't, L>
where
    L: Language,
{
    scope: &'s Scope<L>,
    text: &'t str,
    reductions: Vec<&'static str>,
}

impl<'s, 't, T, L> Parser<'s, 't, T, L>
where
    L: Language,
    T: Debug + Clone + Eq + 'static,
{
    /// Shorthand to create a parser for a nonterminal with a single variant,
    /// parsed by the function `op`.
    ///
    /// The nonterminal-name will be used as the reduction name and added after `op` completes.
    pub fn single_variant(
        scope: &'s Scope<L>,
        text: &'t str,
        nonterminal_name: &'static str,
        mut op: impl FnMut(&mut ActiveVariant<'s, 't, L>) -> Result<T, Set<ParseError<'t>>>,
    ) -> ParseResult<'t, T> {
        Parser::multi_variant(scope, text, nonterminal_name, |parser| {
            parser.parse_variant(nonterminal_name, 0, &mut op);
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
        left_recursion::enter(scope, text, || {
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
            };

            op(&mut parser);

            parser.finish(guard)
        })
    }

    /// Shorthand for `parse_variant` where the parsing operation is to
    /// parse the type `V` and then upcast it to the desired result type.
    pub fn parse_variant_cast<V>(&mut self, variant_precedence: usize)
    where
        V: CoreParse<L> + Upcast<T>,
    {
        let variant_name = std::any::type_name::<V>();
        Self::parse_variant(self, variant_name, variant_precedence, |p| {
            let v: V = p.nonterminal()?;
            Ok(v.upcast())
        })
    }

    /// Parses a single variant for this nonterminal.
    /// The name of the variant is significant and will be tracked as part of the list of reductions.
    /// The precedence is part of how we resolve conflicts: if there are two successful parses with distinct precedence, higher precedence wins.
    /// The `op` is a closure that defines how the variant itself is parsed.
    /// The closure `op` will be invoked with an [`ActiveVariant`][] struct that has methods for consuming identifiers, numbers, keywords, etc.
    pub fn parse_variant(
        &mut self,
        variant_name: &'static str,
        variant_precedence: usize,
        op: impl FnOnce(&mut ActiveVariant<'s, 't, L>) -> Result<T, Set<ParseError<'t>>>,
    ) {
        let span = tracing::span!(
            tracing::Level::TRACE,
            "variant",
            name = variant_name,
            variant_precedence = variant_precedence
        );
        let guard = span.enter();

        let mut active_variant = ActiveVariant {
            scope: self.scope,
            text: self.start_text,
            reductions: vec![],
        };
        let result = op(&mut active_variant);

        // Drop the guard here so that the "success" or "error" results appear outside the variant span.
        // This makes it easier to follow along with parsing when you collapse sections.
        drop(guard);

        match result {
            Ok(value) => {
                active_variant.reductions.push(variant_name);
                self.successes.push((
                    SuccessfulParse {
                        text: active_variant.text,
                        reductions: active_variant.reductions,
                        value,
                    },
                    Precedence(variant_precedence),
                ));
                tracing::trace!("success: {:?}", self.successes.last().unwrap());
            }

            Err(errs) => {
                // If this variant encountered failures, then record them -- but careful!
                // We only record failures where we actually consumed any tokens.
                // This is part of our error reporting and recovery mechanism.
                // Note that we expect (loosely) an LL(1) grammar.
                self.failures.extend(
                    errs.into_iter()
                        .filter(|e| e.consumed_any_since(self.start_text))
                        .inspect(|e| tracing::trace!("error: {e:?}")),
                );
            }
        }
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

        // Otherwise, check if we had an unambiguous parse.
        // This results if there exists some success S that is 'better' than every other success.
        // We say S1 better than S2 if:
        // * S1 has higher precedence OR
        // * S1 parsed more tokens and in the same way (i.e., S2.reductions is a prefix of S1.reductions)
        for (s_i, i) in self.successes.iter().zip(0..) {
            if self
                .successes
                .iter()
                .zip(0..)
                .all(|(s_j, j)| i == j || Self::is_preferable(s_i, s_j))
            {
                let (s_i, _) = self.successes.into_iter().skip(i).next().unwrap();
                // It's better to print this result alongside the main parsing section.
                drop(guard);
                tracing::trace!("best parse = `{:?}`", s_i);
                return Ok(s_i);
            }
        }

        panic!(
            "ambiguous parse of `{text:?}`, possibilities are {possibilities:#?}",
            text = self.start_text,
            possibilities = self.successes,
        );
    }

    fn is_preferable(
        s_i: &(SuccessfulParse<T>, Precedence),
        s_j: &(SuccessfulParse<T>, Precedence),
    ) -> bool {
        let (parse_i, prec_i) = s_i;
        let (parse_j, prec_j) = s_j;

        fn has_prefix<T: Eq>(l1: &[T], l2: &[T]) -> bool {
            l1.len() > l2.len() && (0..l2.len()).all(|i| l1[i] == l2[i])
        }

        prec_i > prec_j || has_prefix(&parse_i.reductions, &parse_j.reductions)
    }
}

impl<'s, 't, L> ActiveVariant<'s, 't, L>
where
    L: Language,
{
    /// The current text remaining to be consumed.
    pub fn text(&self) -> &'t str {
        self.text
    }

    /// Skips whitespace in the input, producing no reduction.
    pub fn skip_whitespace(&mut self) {
        self.text = skip_whitespace(self.text);
    }

    /// Skips a comma in the input, producing no reduction.
    pub fn skip_trailing_comma(&mut self) {
        self.text = skip_trailing_comma(self.text);
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
        let text0 = self.text;
        match self.identifier_like_string() {
            Ok(ident) if &*ident == expected => Ok(()),
            _ => Err(ParseError::at(
                skip_whitespace(text0),
                format!("expected `{}`", expected),
            )),
        }
    }

    /// Reject next identifier-like string if it is one of the given list of keywords.
    /// Does not consume any input.
    /// You can this to implement positional keywords -- just before parsing an identifier or variable,
    /// you can invoke `reject_custom_keywords` to reject anything that you don't want to permit in this position.
    #[tracing::instrument(level = "trace", ret)]
    pub fn reject_custom_keywords(&self, keywords: &[&str]) -> Result<(), Set<ParseError<'t>>> {
        let mut this = ActiveVariant {
            text: self.text,
            reductions: vec![],
            scope: self.scope,
        };

        match this.identifier_like_string() {
            Ok(ident) => {
                if keywords.iter().any(|&kw| kw == ident) {
                    return Err(ParseError::at(
                        self.text,
                        format!("expected identified, found keyword `{ident:?}`"),
                    ));
                }

                Ok(())
            }

            Err(_) => Ok(()),
        }
    }

    /// Extracts a string that meets the regex for an identifier
    /// (but it could also be a keyword).
    #[tracing::instrument(level = "trace", ret)]
    pub fn identifier_like_string(&mut self) -> Result<String, Set<ParseError<'t>>> {
        self.string(
            |ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '_'),
            |ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'),
            "identifier",
        )
    }

    /// Extracts a maximal identifier from the start of text,
    /// following the usual rules. **Disallows language keywords.**
    /// If you want to disallow additional keywords,
    /// see the `reject_custom_keywords` method.
    #[tracing::instrument(level = "trace", ret)]
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
            scope: &scope,
            text: self.text,
            reductions: vec![],
        };
        let result = op(&mut av);
        self.text = av.text;
        self.reductions.extend(av.reductions);
        result
    }

    /// Parses the next identifier as a variable in scope
    /// with the kind appropriate for the return type `R`.
    ///
    /// **NB:** This departs from the limits of context-free
    /// grammars -- the scope is a piece of context that affects how
    /// our parsing proceeds!
    ///
    /// Variables don't implement Parse themselves
    /// because you can't parse a variable into a *variable*,
    /// instead, you parse it into a `Parameter`.
    /// The parsing code then downcasts that parameter into the
    /// type it wants (e.g., in Rust, a `Ty`).
    /// This downcast will fail if the `Parameter` is not of the appropriate
    /// kind (which will result in a parse error).
    ///
    /// This avoids "miskinding", i.e., parsing a `Ty` that contains
    /// a lifetime variable.
    ///
    /// It also allows parsing where you use variables to stand for
    /// more complex parameters, which is kind of combining parsing
    /// and substitution and can be convenient in tests.
    #[tracing::instrument(level = "trace", ret)]
    pub fn variable<R>(&mut self) -> Result<R, Set<ParseError<'t>>>
    where
        R: Debug + DowncastFrom<CoreParameter<L>>,
    {
        self.skip_whitespace();
        let type_name = std::any::type_name::<R>();
        let text0 = self.text;
        let id = self.identifier()?;
        match self.scope.lookup(&id) {
            Some(parameter) => match parameter.downcast() {
                Some(v) => Ok(v),
                None => Err(ParseError::at(
                    text0,
                    format!(
                        "wrong kind, expected a {}, found a {:?}",
                        type_name,
                        parameter.kind()
                    ),
                )),
            },
            None => Err(ParseError::at(text0, format!("unrecognized variable"))),
        }
    }

    /// Extract a number from the input, erroring if the input does not start with a number.
    #[tracing::instrument(level = "trace", ret)]
    pub fn number<T>(&mut self) -> Result<T, Set<ParseError<'t>>>
    where
        T: FromStr + std::fmt::Debug,
    {
        let description = std::any::type_name::<T>();
        let text0 = self.text;
        let s = self.string(char::is_numeric, char::is_numeric, description)?;
        match T::from_str(&s) {
            Ok(t) => Ok(t),
            Err(_) => Err(ParseError::at(
                skip_whitespace(text0),
                format!("invalid number"),
            )),
        }
    }

    /// Consumes a single token from the input after skipping whitespace.
    /// Does not record any reduction.
    /// We don't generally record reductions for methods on parser,
    /// the assumption is that they are implied by the nonterminal name / variant.
    ///
    /// The token is defined by `op`, which is a function that takes
    /// the text and returns a result + new text.
    ///
    /// Tokens do not have internal
    /// reductions, so `op` does not need to return reductions.
    fn token<T>(
        &mut self,
        op: impl FnOnce(&'t str) -> TokenResult<'t, T>,
    ) -> Result<T, Set<ParseError<'t>>> {
        self.skip_whitespace();
        let value;
        (value, self.text) = op(self.text)?;
        Ok(value)
    }

    /// Parse an instance of the given type `T` (after skipping whitespace).
    #[track_caller]
    pub fn nonterminal<T>(&mut self) -> Result<T, Set<ParseError<'t>>>
    where
        T: CoreParse<L>,
        L: Language,
    {
        self.nonterminal_with(T::parse)
    }

    /// Try to parse the current point as `T` and return `None` if there is nothing there.
    ///
    /// **NB:**  If the parse partially succeeds, i.e., we are able to consume some tokens
    /// but cannot completely parse, then this is an error, not a `None` result.
    /// So for example if the grammar for `T` is `()` and the input is `"foo"`,
    /// then we will return `None`. But if the input were `"(foo)"`, we would return an error,
    /// because we consumed the open paren `(` from `T` but then encountered an error
    /// looking for the closing paren `)`.
    #[track_caller]
    #[tracing::instrument(level = "Trace", ret)]
    pub fn opt_nonterminal<T>(&mut self) -> Result<Option<T>, Set<ParseError<'t>>>
    where
        T: CoreParse<L>,
    {
        let text0 = self.text;
        match self.nonterminal() {
            Ok(v) => Ok(Some(v)),
            Err(mut errs) => {
                errs.retain(|e| e.consumed_any_since(text0));
                if errs.is_empty() {
                    // If no errors consumed anything, then self.text
                    // must not have advanced.
                    assert_eq!(skip_whitespace(text0), self.text);
                    Ok(None)
                } else {
                    Err(errs)
                }
            }
        }
    }

    /// Continue parsing instances of `T` while we can.
    /// This is a greedy parse.
    pub fn many_nonterminal<T>(&mut self) -> Result<Vec<T>, Set<ParseError<'t>>>
    where
        T: CoreParse<L>,
    {
        let mut result = vec![];
        while let Some(e) = self.opt_nonterminal()? {
            result.push(e);
        }
        Ok(result)
    }

    #[tracing::instrument(level = "Trace", ret)]
    pub fn delimited_nonterminal<T>(
        &mut self,
        open: char,
        optional: bool,
        close: char,
    ) -> Result<Vec<T>, Set<ParseError<'t>>>
    where
        T: CoreParse<L>,
    {
        // Look for the opening delimiter.
        // If we don't find it, then this is either an empty vector (if optional) or an error (otherwise).
        match self.expect_char(open) {
            Ok(()) => {}
            Err(errs) => {
                return if optional { Ok(vec![]) } else { Err(errs) };
            }
        }

        // Now parse the contents.
        let result = self.comma_nonterminal()?;

        self.expect_char(close)?;

        Ok(result)
    }

    /// Parse multiple instances of `T` separated by commas.
    #[track_caller]
    pub fn comma_nonterminal<T>(&mut self) -> Result<Vec<T>, Set<ParseError<'t>>>
    where
        T: CoreParse<L>,
    {
        let mut result = vec![];
        while let Some(e) = self.opt_nonterminal()? {
            result.push(e);

            if self.expect_char(',').is_err() {
                break;
            }
        }
        Ok(result)
    }

    /// Consumes a nonterminal from the input after skipping whitespace.
    /// The nonterminal is defined by `op`, which is some parse function
    /// that goes from text to a `ParseResult`. The `ParseResult` is assumed
    /// to contain all the reductions (including the final reduction for
    /// the nonterminal itself).
    /// This is rarely used, prefer `nonterminal` when you can.
    #[track_caller]
    pub fn nonterminal_with<T>(
        &mut self,
        op: impl FnOnce(&'s Scope<L>, &'t str) -> ParseResult<'t, T>,
    ) -> Result<T, Set<ParseError<'t>>> {
        self.skip_whitespace();

        let SuccessfulParse {
            text,
            reductions,
            value,
        } = op(self.scope, self.text)?;

        // Adjust our point in the input text
        self.text = text;

        // Some value was produced, so there must have been a reduction
        assert!(!reductions.is_empty());

        // Accumulate the reductions we have done so far
        self.reductions.extend(reductions);

        // And return the value
        Ok(value)
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
