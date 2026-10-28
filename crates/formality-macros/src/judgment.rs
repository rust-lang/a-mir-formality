#![expect(dead_code)]
#![expect(unused_variables)]

use proc_macro2::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::token::Paren;
use syn::{
    braced, parenthesized, parse_macro_input, Attribute, Expr, Ident, LitStr, Pat, Token, Type,
    Visibility,
};

use std::collections::VecDeque;

/// Judgment ::= Signature Body
#[derive(Debug, Clone)]
struct Judgment {
    attributes: Vec<Attribute>,
    signature: Signature,
    body: JudgmentBody,
}

/// Signature ::= Visibility "fn" Identifier "(" { Argument ","} ")" "=>" Type
#[derive(Debug, Clone)]
struct Signature {
    visibility: Visibility,
    name: Ident,
    arguments: Vec<Argument>,
    return_: Type,
}

/// Argument ::= Identifier ":" Type
#[derive(Debug, Clone)]
struct Argument {
    name: Ident,
    ty: Type,
}

/// JudgmentBody ::= Debug Asserts Trivials Rules
#[derive(Debug, Clone)]
struct JudgmentBody {
    debug: Debug,
    asserts: Asserts,
    trivials: Trivials,
    rules: Rules,
}

/// Debug ::= "debug" "(" Identifier "," ")"
#[derive(Debug, Clone)]
struct Debug {
    idents: Vec<Ident>,
}

/// Asserts ::= {Assert}
#[derive(Debug, Clone)]
struct Asserts {
    assertions: Vec<Assert>,
}

/// Assert ::= "assert" "(" Expression ")"
#[derive(Debug, Clone)]
struct Assert {
    assertion: Expr,
}

/// Trivials ::= {Trivial}
#[derive(Debug, Clone)]
struct Trivials {
    trivials: Vec<Trivial>,
}

/// Trivial ::= "trivial" "(" Expression "=>" Expression ")"
#[derive(Debug, Clone)]
struct Trivial {
    hypothesis: Expr,
    conclusion: Expr,
}

/// Rules ::= {"(" Rule ")"}
#[derive(Debug, Clone)]
struct Rules {
    rules: Vec<Rule>,
}

/// Rule ::= Premises "---" {-} "(" String ")" Conclusion
#[derive(Debug, Clone)]
struct Rule {
    premises: Premises,
    name: String,
    conclusion: Conclusion,
}

/// Premises ::= {Premise}
#[derive(Debug, Clone)]
struct Premises {
    premises: VecDeque<Premise>,
}

/// Premise ::= (xxxx)!
#[derive(Debug, Clone)]
struct Premise {
    kind: PremiseKind,
    paren: Paren,
    commit_point: Option<Token![!]>,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
struct IfLet {
    pattern: Pat,
    expression: Expr,
}

#[derive(Debug, Clone)]
struct If {
    expression: Expr,
}

#[derive(Debug, Clone)]
struct ForAll {
    identifier: Ident,
    collection: Expr,
    body: Premises,
}

#[derive(Debug, Clone)]
struct ForAllWith {
    identifier: Ident,
    collection: Expr,
    state: Vec<Ident>,
    body: Premises,
}

#[derive(Debug, Clone)]
struct Fail {
    message: LitStr,
    expressions: Vec<Expr>,
}

#[derive(Debug, Clone)]
struct Implication {
    hypothesis: Expr,
    conclusion: Pat,
}

#[derive(Debug, Clone)]
struct Let {
    binding: Binding,
    expression: Expr,
}

#[derive(Debug, Clone)]
struct LetForAll {
    binding: Binding,
    // TODO: Better naming
    looper: Ident,
    expression: Expr,
    body: Premises,
}

#[derive(Debug, Clone)]
struct Scope;

#[derive(Debug, Clone)]
enum Binding {
    Pattern { pattern: Pat },
    Identifier { identifier: Ident, ty: Option<Type> },
}

/// Conclusion ::= Identifier "(" { Pattern } ")" "=>" Expression
#[derive(Debug, Clone)]
struct Conclusion {
    name: Ident,
    patterns: Vec<Pat>,
    result: Expr,
}

impl Judgment {
    fn name(&self) -> &Ident {
        &self.signature.name
    }

    fn input_tys(&self) -> Vec<&Type> {
        self.signature
            .arguments
            .iter()
            .map(|Argument { name, ty }| ty)
            .collect()
    }

    fn input_names(&self) -> Vec<&Ident> {
        self.signature
            .arguments
            .iter()
            .map(|Argument { name, ty }| name)
            .collect()
    }

    fn output(&self) -> &Type {
        &self.signature.return_
    }

    fn debug_input_names(&self) -> &Vec<Ident> {
        &self.body.debug.idents
    }

    fn assertions(&self) -> Vec<&Expr> {
        self.body
            .asserts
            .assertions
            .iter()
            .map(|Assert { assertion }| assertion)
            .collect()
    }

    fn trivials_exprs(&self) -> Vec<&Expr> {
        self.body
            .trivials
            .trivials
            .iter()
            .map(
                |Trivial {
                     hypothesis,
                     conclusion,
                 }| hypothesis,
            )
            .collect()
    }

    fn trivials_results(&self) -> Vec<&Expr> {
        self.body
            .trivials
            .trivials
            .iter()
            .map(
                |Trivial {
                     hypothesis,
                     conclusion,
                 }| conclusion,
            )
            .collect()
    }

    fn rules(&self) -> Vec<&Rule> {
        self.body.rules.rules.iter().collect()
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
            parse_for_all(&mut &inner)?
        } else if inner.peek(keyword::fail) {
            todo!()
        } else if inner.peek(keyword::scope) {
            PremiseKind::Scope(inner.parse()?)
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

fn check_judgment(judgment: &Judgment) {
    // TODO: Replace panic! with compile_error! and respan
    for rule in judgment.rules() {
        if rule.conclusion.name != *judgment.name() {
            panic!("Wrong judgment name in conclusion");
        }

        if rule.conclusion.patterns.len() < judgment.signature.arguments.len() {
            panic!("fewer patterns in rule than arguments on fn");
        }

        if rule.conclusion.patterns.len() > judgment.signature.arguments.len() {
            panic!("more patterns in rule than arguments on fn");
        }
    }
}

fn emit(judgment: Judgment) -> TokenStream {
    let attributes = emit_attributes(&judgment.attributes);
    let signature_tokens = emit_signature(&judgment.signature);
    let judgment_struct = emit_judgment_struct(&judgment);
    let function_body = emit_function_body(&judgment);

    quote! {
        #attributes
        #signature_tokens {
            #judgment_struct

            #function_body
        }
    }
}

fn emit_attributes(attributes: &Vec<Attribute>) -> TokenStream {
    quote! {
        #(#attributes)*
    }
}

fn emit_signature(
    Signature {
        visibility,
        name,
        arguments,
        return_,
    }: &Signature,
) -> TokenStream {
    let args: Vec<_> = arguments
        .iter()
        .map(|Argument { name, ty }| quote! { #name: impl ::formality_core::Upcast<#ty> })
        .collect();

    quote! {
        #visibility fn #name(#(#args),*) -> ::formality_core::ProvenSet<#return_>
    }
}

fn emit_judgment_struct(judgment: &Judgment) -> TokenStream {
    let input_tys: Vec<_> = judgment.input_tys();
    let input_names = judgment.input_names();

    let name = judgment.name();
    let debug_input_names = judgment.debug_input_names();

    quote! {
        #[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Clone)]
        struct __JudgmentStruct(#(#input_tys),*);

        ::formality_core::cast_impl!(__JudgmentStruct);

        impl std::fmt::Debug for __JudgmentStruct {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut f = fmt.debug_struct(stringify!(#name));
                let __JudgmentStruct(#(#input_names),*) = self;
                #(
                    f.field(stringify!(#debug_input_names), #debug_input_names);
                )*
                f.finish()
            }
        }
    }
}

fn emit_function_body(judgment: &Judgment) -> TokenStream {
    let input_name: Vec<_> = judgment.input_names();
    let input_ty: Vec<_> = judgment.input_tys();
    let output = judgment.output();
    let name = judgment.name();
    let debug_input_name = judgment.debug_input_names();

    let assertions = emit_assertions(judgment);
    let trivials = emit_trivials(judgment);
    let rules = emit_rules(judgment);

    quote! {
        #(let #input_name: #input_ty = ::formality_core::Upcast::upcast(#input_name);)*

        #assertions
        #trivials

        let mut failed_rules = ::formality_core::set![];
        let input = __JudgmentStruct(#(#input_name),*);
        let output = ::formality_core::fixed_point::fixed_point::<
            __JudgmentStruct,
        ::formality_core::Map<#output, ::formality_core::judgment::ProofTree>,
        >(
            // Tracing span:
            |input| {
                let __JudgmentStruct(#(#input_name),*) = input;
                tracing::debug_span!(
                    stringify!(#name),
                    #(?#debug_input_name),*
                )
            },

            // Stack:
            {
                thread_local! {
                    static R: ::formality_core::judgment::JudgmentStack<__JudgmentStruct, #output> = Default::default()
                }
                &R
            },

            // Input:
            input.clone(),

            // Default value:
            |_| Default::default(),

            // Next value:
            |input: __JudgmentStruct| {
                let mut output: ::formality_core::Map<#output, ::formality_core::judgment::ProofTree> = ::formality_core::Map::new();

                failed_rules.clear();

                #[allow(unused)]
                let input_string = format!("{:?}", input);
                #rules
                output
            },
        );

        if !output.is_empty() {
            ::formality_core::ProvenSet::proven(output)
        } else {
            ::formality_core::ProvenSet::failed_rules(
                &input,
                ::formality_core::judgment::FailureLocation::caller(),
                failed_rules,
            )
        }
    }
}

fn emit_assertions(judgment: &Judgment) -> TokenStream {
    let input_name: Vec<_> = judgment.input_names();
    let assert_expr: Vec<_> = judgment.assertions();
    quote! {
        {
            #(
                #[allow(unused_variables)]
                let #input_name = &#input_name;
            )*
            #(
                ::formality_core::respan!(
                    #assert_expr
                    (
                        ::formality_core::judgment::JudgmentAssertion::assert(#assert_expr, stringify!(#assert_expr));
                    )
                )
            )*
        }
    }
}

fn emit_trivials(judgment: &Judgment) -> TokenStream {
    let trivial_expr: Vec<_> = judgment.trivials_exprs();
    let trivial_result: Vec<_> = judgment.trivials_results();

    quote! {
        #(
            // Trivial cases are an (important) optimization that lets
            // you cut out all the normal rules.
            if #trivial_expr {
                let trivial_result = #trivial_result;
                let (file, line, column) = ::formality_core::respan!(
                    #trivial_expr
                    ((file!(), line!(), column!()))
                );
                let proof_tree = ::formality_core::judgment::ProofTree::with_all(
                    format!("trivial, as {} is true: {trivial_result:?}", stringify!(#trivial_expr)),
                    Default::default(),
                    None,
                    file,
                    line,
                    column,
                    Default::default(),
                );
                return ::formality_core::ProvenSet::singleton((trivial_result, proof_tree));
            }
        )*
    }
}

fn emit_rules(judgment: &Judgment) -> TokenStream {
    let rules: Vec<_> = judgment
        .body
        .rules
        .rules
        .iter()
        .map(|rule| emit_rule(judgment, rule.clone()))
        .collect();

    quote! {
        #(#rules)*
    }
}

fn emit_rule(judgment: &Judgment, rule: Rule) -> TokenStream {
    let input_names = judgment.input_names();
    let judgment_name = judgment.name();
    let conclusion_name = rule.conclusion.name.clone();
    let rule_name = rule.name.clone();

    // TODO: For the moment always true.
    // Must think about when working with the commit points
    // __matched is updated, from true to false, if a commit point (!) is found.
    let match_default = true;

    let premises = emit_premises(judgment, rule.clone());
    // TODO: Maybe move to its own function
    let rule_stream = quote! {
        tracing::trace_span!("matched rule", rule = #rule_name, judgment = stringify!(#judgment_name)).in_scope(|| {
            tracing::debug!("matched rule {:?}", #rule_name);
            #[allow(unused_mut)]
            let mut child_proof_trees: Vec<::formality_core::judgment::ProofTree> = Vec::new();

            #premises
        });
    };
    let match_and_rule = emit_match(judgment, rule, rule_stream);

    quote! {
        {
            ::formality_macros::respan!(
                #conclusion_name
                (
                    // give the user a type error if the name they gave
                    // in the conclusion is not the same as the name of the
                    // function
                    #[allow(dead_code)]
                    struct WrongJudgmentNameInConclusion;
                    const _: WrongJudgmentNameInConclusion = {
                        let #judgment_name = WrongJudgmentNameInConclusion;
                        #conclusion_name
                    };
                )
            );

            if let Some(__JudgmentStruct(#(#input_names),*)) = Some(&input) {
                let __judgment_name = stringify!(#judgment_name);
                let __attributes: Vec<(String, String)> = vec![
                    #((stringify!(#input_names).to_string(), format!("{:?}", #input_names))),*
                ];
                let mut __matched = #match_default;
                #match_and_rule
            }
        }
    }
}

fn emit_match(judgment: &Judgment, rule: Rule, mut rule_stream: TokenStream) -> TokenStream {
    for (pattern, Argument { name, ty }) in rule
        .conclusion
        .patterns
        .iter()
        .zip(&judgment.signature.arguments)
        .rev()
    {
        match pattern {
            Pat::Path(expr_path) => {
                rule_stream = quote! {
                    if let Some(#pattern) = &::formality_core::Downcast::downcast(#name) {
                        #rule_stream
                    }
                };
            }
            Pat::Struct(pat_struct) => todo!(),
            Pat::Tuple(pat_tuple) => todo!(),
            Pat::TupleStruct(pat_tuple_struct) => {
                rule_stream = quote! {
                    if let Some(#pattern) = &::formality_core::Downcast::downcast(#name) {
                        #rule_stream
                    }
                };
            }

            Pat::Ident(pat_ident) => {
                rule_stream = quote! { { let #pat_ident = #name; #rule_stream }}
            }
            Pat::Const(expr_const) => todo!(),
            Pat::Lit(expr_lit) => todo!(),
            Pat::Macro(expr_macro) => todo!(),
            Pat::Or(pat_or) => todo!(),
            Pat::Paren(pat_paren) => todo!(),
            Pat::Range(expr_range) => todo!(),
            Pat::Reference(pat_reference) => todo!(),
            Pat::Rest(pat_rest) => todo!(),
            Pat::Slice(pat_slice) => todo!(),
            Pat::Type(pat_type) => todo!(),
            Pat::Verbatim(token_stream) => todo!(),
            Pat::Wild(pat_wild) => todo!(),
            _ => todo!(),
        }
    }

    rule_stream
}

fn emit_premises(judgment: &Judgment, mut rule: Rule) -> TokenStream {
    let Some(premise) = rule.premises.premises.pop_front() else {
        return emit_collect(judgment, rule);
    };

    match premise.kind {
        PremiseKind::IfLet(_) => todo!("if let"),
        PremiseKind::If(_) => todo!("if"),
        PremiseKind::ForAll(for_all) => todo!("for all"),
        PremiseKind::ForAllWith(for_all_with) => todo!("for all with"),
        PremiseKind::Fail(fail) => todo!("fail"),
        PremiseKind::Implication(implication) => emit_implication(judgment, rule, implication),
        PremiseKind::Let(let_) => emit_let(judgment, rule, let_),
        PremiseKind::LetForAll(let_for_all) => todo!("let for all"),
        PremiseKind::Scope(scope) => todo!("scope"),
    }
}

fn emit_let(judgment: &Judgment, rule: Rule, let_: Let) -> TokenStream {
    let record_failure = emit_record_failure(&rule.name);
    let premises = emit_premises(judgment, rule);

    let expression = let_.expression;
    let p = match let_.binding {
        Binding::Pattern { pattern } => quote! { #pattern },
        Binding::Identifier { identifier, ty } => quote! { #identifier},
    };

    quote! {
        match ::formality_core::judgment::try_catch::<_>(|| Ok(#expression)) {
            Ok(p) => {
                let proof_tree = ::formality_core::judgment::ProofTree::leaf(format!("{} = {p:?}", stringify!(#p)));
                let #p = &p;
                child_proof_trees.push(proof_tree);
                #premises
            }

            Err(e) => {
                let cause = e;
                #record_failure
            }
        }
    }
}

fn emit_implication(judgment: &Judgment, rule: Rule, implication: Implication) -> TokenStream {
    let record_failure = emit_record_failure(&rule.name);
    let premises = emit_premises(judgment, rule);

    let i = implication.hypothesis;

    quote! {
        match ::formality_core::judgment::CheckProven::check_proven(
            #i,
        ) {
            Ok(proof_tree) => {
                child_proof_trees.push(proof_tree);
                #premises
            }
            Err(e) => {
                let cause = e;
                #record_failure
            }
        }
    }
}

fn emit_record_failure(rule_name: &str) -> TokenStream {
    // failed_rules, __matched, cause are in scope
    // TODO: Update parser, such that the Premise span start with the opening paren and ends at the closing paren.
    let step_expr = quote! {};

    quote! {
        // let file = ::formality_core::respan!(#step_expr (file!()));
        // let line = ::formality_core::respan!(#step_expr (line!()));
        // let column = ::formality_core::respan!(#step_expr (column!()));
        let file = file!();
        let line = line!();
        let column = column!();

        if __matched {
            tracing::debug!(
                "rule {rn} failed because {cause} ({file}:{line}:{column})",
                rn = #rule_name,
                cause = cause,
                file = file,
                line = line,
                column = column,
            );
            failed_rules.insert(
                ::formality_core::judgment::FailedRule {
                    rule_name: Some(#rule_name.to_string()),
                    file: file.to_string(),
                    line: line,
                    column: column,
                    cause,
                }
            );
        } else {
            tracing::trace!(
                "rule {rn} failed because {cause} ({file}:{line}:{column})",
                rn = #rule_name,
                cause = cause,
                file = file,
                line = line,
                column = column,
            );
        }
    }
}

fn emit_collect(judgment: &Judgment, rule: Rule) -> TokenStream {
    let judgment_name = judgment.name();
    let rule_name = rule.name;
    let result = rule.conclusion.result;

    quote! {
        {
            let result = ::formality_core::Upcast::upcast(#result);
            let mut attributes = __attributes.clone();
            attributes.push(("result".to_string(), format!("{:?}", result)));
            let (file, line, column) = ::formality_core::respan!(#rule_name ((file!(), line!(), column!())));
            let proof_tree = ::formality_core::judgment::ProofTree::with_all(
                __judgment_name,
                attributes,
                Some(#rule_name),
                file,
                line,
                column,
                child_proof_trees.clone(),
            );
            tracing::debug!("produced {:?} from rule {:?} in judgment {:?}", result, #rule_name, stringify!(#judgment_name));
            ::formality_core::judgment::insert_smallest_proof(&mut output, result, proof_tree);
        }
    }
}

pub fn judgment(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as Judgment);
    check_judgment(&input);
    let tokens = emit(input);
    proc_macro::TokenStream::from(tokens)
}

mod keyword {
    syn::custom_keyword!(debug);
    syn::custom_keyword!(assert);
    syn::custom_keyword!(trivial);

    syn::custom_keyword!(for_all);
    syn::custom_keyword!(fail);
    syn::custom_keyword!(scope);
}
