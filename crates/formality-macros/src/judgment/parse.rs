use proc_macro2::TokenStream;
use syn::parse::discouraged::Speculative;
use syn::parse::{Parse, ParseStream, Result};
use syn::token::Paren;
use syn::{braced, parenthesized, parse2, Attribute, Expr, Ident, LitStr, Pat, Token};

use std::collections::VecDeque;

use crate::judgment::ast::{
    Argument, Assert, Asserts, Ast, Binding, ComprehensionBody, Comprehesion, Conclusion, Debug,
    Fail, ForAll, If, IfLet, Implication, In, Judgment, JudgmentBody, Let, Premise, PremiseKind,
    Premises, Rule, Rules, Scope, Signature, Trivial, Trivials,
};
use crate::judgment::keyword;

pub fn parse(tokens: TokenStream) -> Ast {
    match parse2(tokens) {
        Ok(ast) => ast,
        Err(e) => panic!("Could not parse judgment: {e}"),
    }
}

impl Parse for Judgment {
    fn parse(input: ParseStream) -> Result<Self> {
        let attributes = input.call(Attribute::parse_outer)?;

        let signature = input.parse()?;

        let body = {
            let content;
            braced!(content in input);
            content.parse()?
        };

        Ok(Judgment {
            attributes,
            signature,
            body,
        })
    }
}

impl Parse for Signature {
    fn parse(input: ParseStream) -> Result<Self> {
        let visibility = input.parse()?;
        input.parse::<Token![fn]>()?;
        let name = input.parse()?;

        let arguments = {
            let content;
            parenthesized!(content in input);
            content
                .parse_terminated(Argument::parse, Token![,])?
                .into_iter()
                .collect()
        };

        input.parse::<Token![=>]>()?;
        let return_ = input.parse()?;

        Ok(Signature {
            visibility,
            name,
            arguments,
            return_,
        })
    }
}

impl Parse for Argument {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        input.parse::<Token![:]>()?;
        let ty = input.parse()?;

        Ok(Argument { name, ty })
    }
}

impl Parse for JudgmentBody {
    fn parse(input: ParseStream) -> Result<Self> {
        let debug = input.parse()?;
        let asserts = input.parse()?;
        let trivials = input.parse()?;
        let rules = input.parse()?;

        Ok(JudgmentBody {
            debug,
            asserts,
            trivials,
            rules,
        })
    }
}

impl Parse for Debug {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<keyword::debug>()?;

        let idents = {
            let content;
            parenthesized!(content in input);
            content
                .parse_terminated(Ident::parse, Token![,])?
                .into_iter()
                .collect()
        };

        Ok(Debug { idents })
    }
}

impl Parse for Asserts {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut assertions = Vec::new();

        while input.peek(keyword::assert) {
            assertions.push(input.parse()?);
        }

        Ok(Asserts { assertions })
    }
}

impl Parse for Assert {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<keyword::assert>()?;

        let assertion = {
            let content;
            parenthesized!(content in input);
            content.parse()?
        };

        Ok(Assert { assertion })
    }
}

impl Parse for Trivials {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut trivials = Vec::new();

        while input.peek(keyword::trivial) {
            trivials.push(input.parse()?);
        }

        Ok(Trivials { trivials })
    }
}

impl Parse for Trivial {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<keyword::trivial>()?;

        let hypothesis;
        let conclusion;
        {
            let content;
            parenthesized!(content in input);

            hypothesis = content.parse()?;
            content.parse::<Token![=>]>()?;
            conclusion = content.parse()?;
        };

        Ok(Trivial {
            hypothesis,
            conclusion,
        })
    }
}

impl Parse for Rules {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut rules = Vec::new();

        while input.peek(Paren) {
            let inner;
            parenthesized!(inner in input);
            rules.push(inner.parse()?);
        }

        Ok(Rules { rules })
    }
}

impl Parse for Rule {
    fn parse(input: ParseStream) -> Result<Self> {
        let premises: Premises = input.parse()?;

        input.parse::<Token![-]>()?;
        input.parse::<Token![-]>()?;
        input.parse::<Token![-]>()?;

        while input.peek(Token![-]) {
            input.parse::<Token![-]>()?;
        }

        let name = {
            let named;
            parenthesized!(named in input);
            named.parse::<LitStr>()?.value()
        };

        let conclusion: Conclusion = input.parse()?;

        Ok(Rule {
            premises,
            name,
            conclusion,
        })
    }
}

impl Parse for Premises {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut premises = VecDeque::new();
        while input.peek(Paren) {
            premises.push_back(input.parse()?);
        }

        Ok(Premises { premises })
    }
}

impl Parse for Premise {
    fn parse(input: ParseStream) -> Result<Self> {
        let paren;
        let kind = {
            let inner;
            paren = parenthesized!(inner in input);

            if inner.peek(Token![if]) && inner.peek2(Token![let]) {
                PremiseKind::IfLet(inner.parse()?)
            } else if inner.peek(Token![if]) {
                PremiseKind::If(inner.parse()?)
            } else if inner.peek(Token![let]) {
                let fork = inner.fork();
                if let Ok(comprehension) = fork.parse::<Comprehesion>() {
                    inner.advance_to(&fork);
                    PremiseKind::Comprehension(comprehension)
                } else {
                    PremiseKind::Let(inner.parse()?)
                }
            } else if inner.peek(keyword::for_all) {
                PremiseKind::ForAll(inner.parse()?)
            } else if inner.peek(keyword::fail) {
                PremiseKind::Fail(inner.parse()?)
            } else if inner.peek(keyword::scope) {
                PremiseKind::Scope(inner.parse()?)
            } else if inner.peek2(Token![in]) {
                PremiseKind::In(inner.parse()?)
            } else {
                PremiseKind::Implication(inner.parse()?)
            }
        };

        let commit_point = if input.peek(Token![!]) {
            Some(input.parse()?)
        } else {
            None
        };

        Ok(Premise {
            kind,
            paren,
            commit_point,
        })
    }
}

impl Parse for IfLet {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![if]>()?;
        input.parse::<Token![let]>()?;
        let pattern = Pat::parse_multi_with_leading_vert(input)?;
        input.parse::<Token![=]>()?;
        let expression = input.parse()?;

        Ok(IfLet {
            pattern,
            expression,
        })
    }
}

impl Parse for If {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![if]>()?;
        let expression = input.parse()?;

        Ok(If { expression })
    }
}

impl Parse for ForAll {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<keyword::for_all>()?;

        let identifier;
        let collection;
        let state;
        {
            let inner;
            parenthesized!(inner in input);
            identifier = inner.parse()?;
            inner.parse::<Token![in]>()?;
            collection = inner.parse()?;

            state = if input.peek(keyword::with) {
                input.parse::<keyword::with>()?;

                {
                    let with;
                    parenthesized!(with in input);

                    with.parse_terminated(Ident::parse, Token![,])?
                        .into_iter()
                        .collect()
                }
            } else {
                vec![]
            };
        }

        let body: Premises = input.parse()?;

        Ok(ForAll {
            identifier,
            collection,
            body,
            state,
        })
    }
}

impl Parse for Fail {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut expressions = vec![];

        input.parse::<keyword::fail>()?;
        let message = input.parse()?;

        if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
            loop {
                if input.fork().parse::<Expr>().is_ok() {
                    expressions.push(input.parse::<Expr>()?);
                } else {
                    break;
                }
            }
        }

        Ok(Fail {
            message,
            expressions,
        })
    }
}

impl Parse for Implication {
    fn parse(input: ParseStream) -> Result<Self> {
        let hypothesis = input.parse()?;
        input.parse::<Token![=]>()?;
        input.parse::<Token![>]>()?;
        let conclusion = Pat::parse_multi_with_leading_vert(input)?;

        Ok(Implication {
            hypothesis,
            conclusion,
        })
    }
}

impl Parse for Let {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![let]>()?;
        let binding = input.parse()?;
        input.parse::<Token![=]>()?;
        let expression = input.parse()?;

        Ok(Let {
            binding,
            expression,
        })
    }
}

impl Parse for Comprehesion {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![let]>()?;
        let binding = input.parse()?;
        input.parse::<Token![=]>()?;

        input.parse::<keyword::for_all>()?;

        let loop_var = input.parse()?;
        input.parse::<Token![in]>()?;
        let collection = input.parse()?;
        input.parse::<Token![=]>()?;
        input.parse::<Token![>]>()?;

        let body = input.parse()?;

        Ok(Comprehesion {
            binding,
            loop_var,
            collection,
            body,
        })
    }
}

impl Parse for ComprehensionBody {
    fn parse(input: ParseStream) -> Result<Self> {
        // TODO: is this really correct?
        // As far as I understand this, the body are premises, but the last premise must be an implication or a value.
        // Everything else is an error, and should be reported.
        let premises = input.parse()?;
        let conclusion = input.parse()?;

        Ok(ComprehensionBody {
            premises,
            conclusion,
        })
    }
}

impl Parse for In {
    fn parse(input: ParseStream) -> Result<Self> {
        let pattern = Pat::parse_multi_with_leading_vert(input)?;
        input.parse::<Token![in]>()?;
        let expression = input.parse()?;

        Ok(In {
            pattern,
            expression,
        })
    }
}

impl Parse for Scope {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<keyword::scope>()?;

        let expression;
        let pattern;
        {
            let scope;
            parenthesized!(scope in input);
            expression = scope.parse()?;
            scope.parse::<Token![=]>()?;
            scope.parse::<Token![>]>()?;
            pattern = Pat::parse_multi_with_leading_vert(&scope)?;
        }

        input.parse::<keyword::with>()?;

        let state = {
            let inner;
            parenthesized!(inner in input);
            inner
                .parse_terminated(Ident::parse, Token![,])?
                .into_iter()
                .collect()
        };

        let body: Premises = input.parse()?;

        Ok(Scope {
            expression,
            inside: pattern,
            state,
            body,
        })
    }
}

impl Parse for Conclusion {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        parenthesized!(content in input);

        let name: Ident = content.parse()?;

        let patterns = {
            let inner;
            parenthesized!(inner in content);
            inner
                .parse_terminated(Pat::parse_multi_with_leading_vert, Token![,])?
                .into_iter()
                .collect()
        };

        content.parse::<Token![=]>()?;
        content.parse::<Token![>]>()?;

        let result: Expr = content.parse()?;

        Ok(Conclusion {
            name,
            patterns,
            result,
        })
    }
}

impl Parse for Binding {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(if input.peek2(Token![:]) {
            let identifier = input.parse()?;
            let ty = if input.peek(Token![:]) {
                input.parse::<Token![:]>()?;
                let ty = input.parse()?;
                Some(ty)
            } else {
                None
            };
            Binding::Identifier { identifier, ty }
        } else {
            Binding::Pattern {
                pattern: Pat::parse_multi_with_leading_vert(input)?,
            }
        })
    }
}
