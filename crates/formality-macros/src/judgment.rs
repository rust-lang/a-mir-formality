#![expect(dead_code)]
#![expect(unused_variables)]

use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::token::Paren;
use syn::{
    braced, parenthesized, parse_macro_input, Attribute, Expr, Ident, LitStr, Pat, Token, Type,
    Visibility,
};

/// Judgment ::= Signature Body
#[derive(Debug)]
struct Judgment {
    attributes: Vec<Attribute>,
    signature: Signature,
    body: JudgmentBody,
}

/// Signature ::= Visibility "fn" Identifier "(" { Argument ","} ")" "=>" Type
#[derive(Debug)]
struct Signature {
    visibility: Visibility,
    name: Ident,
    arguments: Vec<Argument>,
    return_: Type,
}

/// Argument ::= Identifier ":" Type
#[derive(Debug)]
struct Argument {
    name: Ident,
    ty: Type,
}

/// JudgmentBody ::= Debug Asserts Trivials Rules
#[derive(Debug)]
struct JudgmentBody {
    debug: Debug,
    asserts: Asserts,
    trivials: Trivials,
    rules: Rules,
}

/// Debug ::= "debug" "(" Identifier "," ")"
#[derive(Debug)]
struct Debug {
    idents: Vec<Ident>,
}

/// Asserts ::= {Assert}
#[derive(Debug)]
struct Asserts {
    assertions: Vec<Assert>,
}

/// Assert ::= "assert" "(" Expression ")"
#[derive(Debug)]
struct Assert {
    assertion: Expr,
}

/// Trivials ::= {Trivial}
#[derive(Debug)]
struct Trivials {
    trivials: Vec<Trivial>,
}

/// Trivial ::= "trivial" "(" Expression "=>" Expression ")"
#[derive(Debug)]
struct Trivial {
    hypothesis: Expr,
    conclusion: Expr,
}

/// Rules ::= {"(" Rule ")"}
#[derive(Debug)]
struct Rules {
    rules: Vec<Rule>,
}

/// Rule ::= Premises "---" {-} "(" String ")" Conclusion
#[derive(Debug)]
struct Rule {
    premises: Premises,
    name: String,
    conclusion: Conclusion,
}

/// Premises ::= {Premise}
#[derive(Debug)]
struct Premises {
    premises: Vec<Premise>,
}

/// Premise ::=
#[derive(Debug)]
struct Premise {
    kind: PremiseKind,
}

#[derive(Debug)]
enum PremiseKind {
    IfLet(IfLet),
    If(If),
    ForAll(ForAll),
    ForAllWith(ForAllWith),
    Fail(Fail),
    Implication(Implication),
    Let(Let),
    LetForAll(LetForAll),
    // TODO: Define Structure
    Scope(Scope),
}

#[derive(Debug)]
struct IfLet {
    pattern: Pat,
    expression: Expr,
}

#[derive(Debug)]
struct If {
    expression: Expr,
}

#[derive(Debug)]
struct ForAll {
    identifier: Ident,
    collection: Expr,
    body: Premises,
}

#[derive(Debug)]
struct ForAllWith {
    identifier: Ident,
    collection: Expr,
    state: Vec<Ident>,
    body: Premises,
}

#[derive(Debug)]
struct Fail {
    message: LitStr,
    expressions: Vec<Expr>,
}

#[derive(Debug)]
struct Implication {
    hypothesis: Expr,
    conclusion: Pat,
}

#[derive(Debug)]
struct Let {
    binding: Binding,
    expression: Expr,
}

#[derive(Debug)]
struct LetForAll {
    binding: Binding,
    // TODO: Better naming
    looper: Ident,
    expression: Expr,
    body: Premises,
}

#[derive(Debug)]
struct Scope;

#[derive(Debug)]
enum Binding {
    Pattern { pattern: Pat },
    Identifier { identifier: Ident, ty: Option<Type> },
}

/// Conclusion ::= Identifier "(" { Pattern } ")" "=>" Expression
#[derive(Debug)]
struct Conclusion {
    name: Ident,
    patterns: Vec<Pat>,
    result: Expr,
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
        let mut premises = Vec::new();
        while input.peek(Paren) {
            let inner;
            parenthesized!(inner in input);
            premises.push(inner.parse()?);
        }

        Ok(Premises { premises })
    }
}

impl Parse for Premise {
    fn parse(mut input: ParseStream) -> Result<Self> {
        let kind = if input.peek(Token![if]) && input.peek2(Token![let]) {
            PremiseKind::IfLet(input.parse()?)
        } else if input.peek(Token![if]) {
            PremiseKind::If(input.parse()?)
        } else if input.peek(Token![let]) {
            parse_let(&mut input)?
        } else if input.peek(keyword::for_all) {
            parse_for_all(&mut input)?
        } else if input.peek(keyword::fail) {
            todo!()
        } else if input.peek(keyword::scope) {
            PremiseKind::Scope(input.parse()?)
        } else {
            PremiseKind::Implication(input.parse()?)
        };

        Ok(Premise { kind })
    }
}

impl Parse for IfLet {
    fn parse(input: ParseStream) -> Result<Self> {
        todo!()
    }
}

impl Parse for If {
    fn parse(input: ParseStream) -> Result<Self> {
        todo!()
    }
}

impl Parse for ForAll {
    fn parse(input: ParseStream) -> Result<Self> {
        todo!()
    }
}

impl Parse for ForAllWith {
    fn parse(input: ParseStream) -> Result<Self> {
        todo!()
    }
}

impl Parse for Fail {
    fn parse(input: ParseStream) -> Result<Self> {
        todo!()
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
    let looper: Ident = input.parse()?;
    input.parse::<Token![in]>()?;
    let expression: Expr = input.parse()?;
    input.parse::<Token![=]>()?;
    input.parse::<Token![>]>()?;
    let body: Premises = input.parse()?;

    Ok(LetForAll {
        binding,
        looper,
        expression,
        body,
    })
}

fn parse_for_all(input: &mut ParseStream) -> Result<PremiseKind> {
    input.parse::<keyword::for_all>()?;

    let inner;
    parenthesized!(inner in input);
    let identifier: Ident = inner.parse()?;
    inner.parse::<Token![in]>()?;
    let collection: Expr = inner.parse()?;

    let body: Premises = input.parse()?;

    Ok(PremiseKind::ForAll(ForAll {
        identifier,
        collection,
        body,
    }))
}

pub fn judgment(input: TokenStream) -> TokenStream {
    let _input = parse_macro_input!(input as Judgment);
    TokenStream::new()
}

mod keyword {
    syn::custom_keyword!(debug);
    syn::custom_keyword!(assert);
    syn::custom_keyword!(trivial);

    syn::custom_keyword!(for_all);
    syn::custom_keyword!(fail);
    syn::custom_keyword!(scope);
}
