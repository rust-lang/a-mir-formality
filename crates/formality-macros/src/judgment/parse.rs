use proc_macro2::TokenStream;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::token::Paren;
use syn::{
    braced, parenthesized, parse2, Attribute, Expr, Ident, LitStr, Pat, Token, Type, Visibility,
};

use std::collections::VecDeque;

use crate::judgment::ast::{
    Argument, Assert, Asserts, Ast, Binding, Conclusion, Debug, Fail, ForAll, If, IfLet,
    Implication, In, Judgment, JudgmentBody, Let, LetForAll, Premise, PremiseKind, Premises, Rule,
    Rules, Scope, Signature, Trivial, Trivials,
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

        let signature: Signature = input.parse()?;

        let content;
        braced!(content in input);
        let body: JudgmentBody = content.parse()?;

        Ok(Judgment {
            attributes,
            signature,
            body,
        })
    }
}

impl Parse for Signature {
    fn parse(input: ParseStream) -> Result<Self> {
        let visibility: Visibility = input.parse()?;
        input.parse::<Token![fn]>()?;
        let name: Ident = input.parse()?;

        // TODO: Can this be improved?
        let content;
        parenthesized!(content in input);
        let arguments: Punctuated<Argument, Token![,]> =
            content.parse_terminated(Argument::parse, Token![,])?;
        let arguments: Vec<Argument> = arguments.into_iter().collect();

        input.parse::<Token![=]>()?;
        input.parse::<Token![>]>()?;
        let return_: Type = input.parse()?;

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
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        let ty: Type = input.parse()?;

        Ok(Argument { name, ty })
    }
}

impl Parse for JudgmentBody {
    fn parse(input: ParseStream) -> Result<Self> {
        let debug: Debug = input.parse()?;
        let asserts: Asserts = input.parse()?;
        let trivials: Trivials = input.parse()?;
        let rules: Rules = input.parse()?;

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

        let content;
        parenthesized!(content in input);
        let idents: Punctuated<Ident, Token![,]> =
            content.parse_terminated(Ident::parse, Token![,])?;

        Ok(Debug {
            idents: idents.into_iter().collect(),
        })
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

        let content;
        parenthesized!(content in input);
        let assertion: Expr = content.parse()?;

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

        let content;
        parenthesized!(content in input);
        let hypothesis: Expr = content.parse()?;
        content.parse::<Token![=]>()?;
        content.parse::<Token![>]>()?;
        let conclusion: Expr = content.parse()?;

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

        let named;
        parenthesized!(named in input);

        let name: LitStr = named.parse()?;
        let name = name.value();
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
        let inner;
        let paren = parenthesized!(inner in input);

        let kind = if inner.peek(Token![if]) && inner.peek2(Token![let]) {
            PremiseKind::IfLet(inner.parse()?)
        } else if inner.peek(Token![if]) {
            PremiseKind::If(inner.parse()?)
        } else if inner.peek(Token![let]) {
            parse_let(&mut &inner)?
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
        todo!("parse if let")
    }
}

impl Parse for If {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![if]>()?;
        let expression: Expr = input.parse()?;

        Ok(If { expression })
    }
}

impl Parse for ForAll {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<keyword::for_all>()?;

        let inner;
        parenthesized!(inner in input);
        let identifier: Ident = inner.parse()?;
        inner.parse::<Token![in]>()?;
        let collection: Expr = inner.parse()?;
        // TODO: Parse optional state
        let state = vec![];

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
        let message: LitStr = input.parse()?;

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
        let hypothesis: Expr = input.parse()?;
        input.parse::<Token![=]>()?;
        input.parse::<Token![>]>()?;
        let conclusion: Pat = Pat::parse_multi_with_leading_vert(input)?;

        Ok(Implication {
            hypothesis,
            conclusion,
        })
    }
}

impl Parse for In {
    fn parse(input: ParseStream) -> Result<Self> {
        let pattern = Pat::parse_multi_with_leading_vert(input)?;
        input.parse::<Token![in]>()?;
        let expression: Expr = input.parse()?;

        Ok(In {
            pattern,
            expression,
        })
    }
}

impl Parse for Scope {
    fn parse(input: ParseStream) -> Result<Self> {
        todo!()
    }
}

impl Parse for Conclusion {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        parenthesized!(content in input);

        let name: Ident = content.parse()?;

        let inner;
        parenthesized!(inner in content);
        let patterns: Punctuated<Pat, Token![,]> =
            inner.parse_terminated(Pat::parse_multi_with_leading_vert, Token![,])?;
        let patterns = patterns.into_iter().collect();

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
            let identifier: Ident = input.parse()?;
            let ty = if input.peek(Token![:]) {
                input.parse::<Token![:]>()?;
                let ty: Type = input.parse()?;
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

fn parse_let(input: &mut ParseStream) -> Result<PremiseKind> {
    input.parse::<Token![let]>()?;
    let binding: Binding = input.parse()?;
    input.parse::<Token![=]>()?;

    Ok(if input.peek(keyword::for_all) {
        PremiseKind::LetForAll(parse_let_forall_postfix(binding, input)?)
    } else {
        let expression: Expr = input.parse()?;
        PremiseKind::Let(Let {
            binding,
            expression,
        })
    })
}

fn parse_let_forall_postfix(binding: Binding, input: &mut ParseStream) -> Result<LetForAll> {
    input.parse::<keyword::for_all>()?;
    let loop_var: Ident = input.parse()?;
    input.parse::<Token![in]>()?;
    let expression: Expr = input.parse()?;
    input.parse::<Token![=]>()?;
    input.parse::<Token![>]>()?;
    let body: Premises = input.parse()?;

    Ok(LetForAll {
        binding,
        loop_var,
        expression,
        body,
    })
}
