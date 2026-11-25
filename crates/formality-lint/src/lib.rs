//! Token-based checks inside `judgment_fn!` invocations.
//!
//! Rust tokenization handles comments, literals, and nested delimiters. We walk
//! the token trees without parsing Rust syntax or the judgment DSL.

use annotate_snippets::{AnnotationKind, Level, Renderer, Snippet};
use proc_macro2::{Delimiter, Spacing, TokenStream, TokenTree};
use std::ops::Range;

/// A source diagnostic with one-based line and character-column numbers.
#[derive(Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub line: usize,
    pub column: usize,
    pub rule: &'static str,
    pub message: &'static str,
    /// UTF-8 byte range in the original source, for source annotations.
    pub span: Range<usize>,
}

impl Diagnostic {
    /// Render a source annotation without terminal escape codes.
    ///
    /// `source` must be the same text that was passed to [`lint`].
    pub fn render(&self, path: &str, source: &str) -> String {
        let mut group = Level::ERROR
            .primary_title(self.message)
            .id(self.rule)
            .element(
                Snippet::source(source)
                    .path(path)
                    .fold(true)
                    .annotation(AnnotationKind::Primary.span(self.span.clone())),
            );
        if self.rule == "explicit-clone" {
            group = group.element(
                Level::HELP.message("use term constructors or implicit borrowing/casting"),
            );
        }
        Renderer::plain().render(&[group])
    }
}

fn diagnostic(
    source: &str,
    span: Range<usize>,
    rule: &'static str,
    message: &'static str,
) -> Diagnostic {
    let before = &source[..span.start];
    Diagnostic {
        line: before.bytes().filter(|b| *b == b'\n').count() + 1,
        column: before.rsplit('\n').next().unwrap().chars().count() + 1,
        rule,
        message,
        span,
    }
}

/// Check literal macro invocations, including those in test code.
pub fn lint(source: &str) -> Vec<Diagnostic> {
    match source.parse::<TokenStream>() {
        Ok(tokens) => {
            let mut findings = Vec::new();
            lint_tokens(source, tokens, false, &mut findings);
            findings
        }
        Err(error) => vec![diagnostic(
            source,
            error.span().byte_range(),
            "scan-error",
            "could not tokenize Rust source",
        )],
    }
}

fn is_ident(token: &TokenTree, name: &str) -> bool {
    match token {
        TokenTree::Ident(ident) => {
            let spelling = ident.to_string();
            spelling.strip_prefix("r#").unwrap_or(&spelling) == name
        }
        _ => false,
    }
}

fn is_punct(token: &TokenTree, ch: char) -> bool {
    matches!(token, TokenTree::Punct(punct) if punct.as_char() == ch)
}

fn lint_tokens(
    source: &str,
    stream: TokenStream,
    in_judgment: bool,
    findings: &mut Vec<Diagnostic>,
) {
    let tokens: Vec<_> = stream.into_iter().collect();
    for (i, token) in tokens.iter().enumerate() {
        if in_judgment && is_ident(token, "clone") && i > 0 {
            let is_call = matches!(tokens.get(i + 1), Some(TokenTree::Group(group))
                if group.delimiter() == Delimiter::Parenthesis);
            let start = if is_punct(&tokens[i - 1], '.') {
                Some(i - 1)
            } else if i >= 2
                && matches!(&tokens[i - 2], TokenTree::Punct(punct)
                    if punct.as_char() == ':' && punct.spacing() == Spacing::Joint)
                && is_punct(&tokens[i - 1], ':')
            {
                Some(i - 2)
            } else {
                None
            };
            if let Some(start) = start.filter(|_| is_call) {
                findings.push(diagnostic(
                    source,
                    tokens[start].span().byte_range().start..token.span().byte_range().end,
                    "explicit-clone",
                    "explicit clone call in judgment",
                ));
            }
        }
        if let TokenTree::Group(group) = token {
            let is_judgment =
                i >= 2 && is_ident(&tokens[i - 2], "judgment_fn") && is_punct(&tokens[i - 1], '!');
            lint_tokens(source, group.stream(), in_judgment || is_judgment, findings);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn render_clone_with_source_and_help() {
        let source = "judgment_fn! {\n    (let expr = value.clone())\n}\n";
        let finding = lint(source).remove(0);
        assert_eq!(&source[finding.span.clone()], ".clone");
        expect_test::expect![[r#"
            error[explicit-clone]: explicit clone call in judgment
             --> example.rs:2:22
              |
            2 |     (let expr = value.clone())
              |                      ^^^^^^
              |
              = help: use term constructors or implicit borrowing/casting"#]]
        .assert_eq(&finding.render("example.rs", source));
    }

    #[test]
    fn render_scan_error_at_end_of_file() {
        let source = "judgment_fn! {";
        let finding = lint(source).remove(0);
        expect_test::expect![[r#"
            error[scan-error]: could not tokenize Rust source
             --> example.rs:1:14
              |
            1 | judgment_fn! {
              |              ^"#]]
        .assert_eq(&finding.render("example.rs", source));
    }

    #[test]
    fn render_unicode_and_multiline_call() {
        let source = "judgment_fn! {\n    (let λ = value.\n        /* λ */ clone ())\n}\n";
        let finding = lint(source).remove(0);
        assert_eq!(&source[finding.span.clone()], ".\n        /* λ */ clone");
        expect_test::expect![[r#"
            error[explicit-clone]: explicit clone call in judgment
             --> unicode.rs:2:19
              |
            2 |       (let λ = value.
              |  ___________________^
            3 | |         /* λ */ clone ())
              | |_____________________^
              |
              = help: use term constructors or implicit borrowing/casting"#]]
        .assert_eq(&finding.render("unicode.rs", source));
    }

    #[test]
    fn render_multiple_findings_independently() {
        let source = "judgment_fn! {\n    (let a = left.clone())\n    (let b = right.clone())\n}";
        let findings = lint(source);
        assert_eq!(findings.len(), 2);
        let output = findings
            .iter()
            .map(|d| d.render("example.rs", source))
            .collect::<Vec<_>>()
            .join("\n\n");
        expect_test::expect![[r#"
            error[explicit-clone]: explicit clone call in judgment
             --> example.rs:2:18
              |
            2 |     (let a = left.clone())
              |                  ^^^^^^
              |
              = help: use term constructors or implicit borrowing/casting

            error[explicit-clone]: explicit clone call in judgment
             --> example.rs:3:19
              |
            3 |     (let b = right.clone())
              |                   ^^^^^^
              |
              = help: use term constructors or implicit borrowing/casting"#]]
        .assert_eq(&output);
    }

    #[test]
    fn only_judgment_bodies() {
        let source = "fn helper() { x.clone(); }\njudgment_fn! {\n    (let x = value.clone())\n}\nfn other() { y.clone(); }\njudgment_fn! { (let y = value.clone()) }\n";
        let found = lint(source);
        assert_eq!(
            found
                .iter()
                .map(|d| (d.line, d.column, d.rule))
                .collect::<Vec<_>>(),
            [(3, 19, "explicit-clone"), (6, 30, "explicit-clone")]
        );
    }

    #[test]
    fn delimiters_in_comments_and_literals() {
        let source = r###"// judgment_fn! { fake.clone() }
const EXAMPLE: &str = "judgment_fn! { fake.clone() }";
judgment_fn! {
    // } fake.clone()
    /* } /* nested */ fake.clone() */
    (let s = "\" } fake.clone()")
    (let r = r##"}"# fake.clone()"##)
    (let b = br#"} fake.clone()"#)
    (let c = c"} fake.clone()")
    (let ch = '}')
    (let ch = '\'')
    (let ch = '\u{7d}')
    (let byte = b'}')
    (let xs = [nested({ () })])
    (check(&'a value) => result)
    (let x = real.clone())
}
outside.clone();
"###;
        let findings = lint(source);
        assert_eq!(findings.len(), 1);
        assert_eq!((findings[0].line, findings[0].rule), (16, "explicit-clone"));
    }

    #[test]
    fn whitespace_and_macro_delimiters() {
        for (open, close) in [('(', ')'), ('[', ']'), ('{', '}')] {
            let source = format!("path::judgment_fn /*comment*/ ! {open}(let x = value . /* note */ clone\n ()){close}");
            assert_eq!(lint(&source).len(), 1);
            assert_eq!(lint(&source)[0].rule, "explicit-clone");
        }
    }

    #[test]
    fn constructors_and_other_macros_are_clean() {
        assert!(lint("judgment_fn! { (let x = Expr::bin_op(lhs, op, rhs)) }").is_empty());
        assert!(lint("other_judgment_fn! { x.clone() }").is_empty());
    }

    #[test]
    fn qualified_clone() {
        for expr in ["Clone::clone(value)", "<T as Clone>::clone(value)"] {
            let found = lint(&format!("judgment_fn! {{ {expr} }}"));
            assert_eq!(found.len(), 1);
            assert_eq!(found[0].rule, "explicit-clone");
        }
    }

    #[test]
    fn malformed_source_is_reported() {
        for source in [
            "judgment_fn! {",
            "judgment_fn! { (] }",
            "judgment_fn! { \"unterminated",
            "r##\"unterminated",
            "/*",
        ] {
            assert_eq!(lint(source).last().unwrap().rule, "scan-error", "{source}");
        }
    }

    #[test]
    fn raw_identifiers_are_linted() {
        let source = "r#judgment_fn! { value.r#clone(); Clone::r#clone(value); }";
        let findings = lint(source);
        assert_eq!(
            findings
                .iter()
                .map(|d| &source[d.span.clone()])
                .collect::<Vec<_>>(),
            [".r#clone", "::r#clone"]
        );
    }

    #[test]
    fn nested_judgments_are_found_once() {
        let source = "mod m { judgment_fn! { outer.clone(); judgment_fn! { inner.clone(); } } helper.clone(); }";
        let findings = lint(source);
        assert_eq!(findings.len(), 2);
        assert!(findings.iter().all(|d| d.rule == "explicit-clone"));
        assert_eq!(findings[0].span.start, source.find(".clone").unwrap());
        assert_eq!(
            findings[1].span.start,
            source.rfind("inner.clone").unwrap() + 5
        );
    }

    #[test]
    fn literals_and_token_boundaries_do_not_form_calls() {
        // These need only be valid token trees: judgment bodies are a DSL.
        let source = r#"judgment_fn! {
            value . "ignored" clone();
            value.clone "ignored" ();
            value.cloned();
            value.clone;
            Clone: :clone(value);
            /// fake.clone()
            (let x = 1)
        }"#;
        assert!(lint(source).is_empty());
    }

    #[test]
    fn malformed_source_outside_judgment_is_reported() {
        let findings = lint("fn helper() { ] }");
        assert_eq!(findings.len(), 1);
        assert_eq!(findings[0].rule, "scan-error");
    }

    #[test]
    fn unicode_offsets_are_preserved() {
        let source = "judgment_fn! { (let s = \"λ\") (let λ = value.clone()) }";
        let found = lint(source);
        assert_eq!(found.len(), 1);
        assert_eq!(
            found[0].column,
            source[..source.find(".clone").unwrap()].chars().count() + 1
        );
    }
}
