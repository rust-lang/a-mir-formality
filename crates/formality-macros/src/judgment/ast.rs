use syn::token::Paren;
use syn::{Attribute, Expr, Ident, LitStr, Pat, Token, Type, Visibility};

use std::collections::VecDeque;

pub type Ast = Judgment;

/// Judgment ::= Signature Body
#[derive(Debug, Clone)]
pub struct Judgment {
    pub attributes: Vec<Attribute>,
    pub signature: Signature,
    pub body: JudgmentBody,
}

/// Signature ::= Visibility "fn" Identifier "(" { Argument ","} ")" "=>" Type
#[derive(Debug, Clone)]
pub struct Signature {
    pub visibility: Visibility,
    pub name: Ident,
    pub arguments: Vec<Argument>,
    pub return_: Type,
}

/// Argument ::= Identifier ":" Type
#[derive(Debug, Clone)]
pub struct Argument {
    pub name: Ident,
    pub ty: Type,
}

/// JudgmentBody ::= Debug Asserts Trivials Rules
#[derive(Debug, Clone)]
pub struct JudgmentBody {
    pub debug: Debug,
    pub asserts: Asserts,
    pub trivials: Trivials,
    pub rules: Rules,
}

/// Debug ::= "debug" "(" Identifier "," ")"
#[derive(Debug, Clone)]
pub struct Debug {
    pub idents: Vec<Ident>,
}

/// Asserts ::= {Assert}
#[derive(Debug, Clone)]
pub struct Asserts {
    pub assertions: Vec<Assert>,
}

/// Assert ::= "assert" "(" Expression ")"
#[derive(Debug, Clone)]
pub struct Assert {
    pub assertion: Expr,
}

/// Trivials ::= {Trivial}
#[derive(Debug, Clone)]
pub struct Trivials {
    pub trivials: Vec<Trivial>,
}

/// Trivial ::= "trivial" "(" Expression "=>" Expression ")"
#[derive(Debug, Clone)]
pub struct Trivial {
    pub hypothesis: Expr,
    pub conclusion: Expr,
}

/// Rules ::= {"(" Rule ")"}
#[derive(Debug, Clone)]
pub struct Rules {
    pub rules: Vec<Rule>,
}

/// Rule ::= Premises "---" {-} "(" String ")" Conclusion
#[derive(Debug, Clone)]
pub struct Rule {
    pub premises: Premises,
    pub name: String,
    pub conclusion: Conclusion,
}

/// Premises ::= {Premise}
#[derive(Debug, Clone)]
pub struct Premises {
    pub premises: VecDeque<Premise>,
}

/// Premise ::= (xxxx)!
#[derive(Debug, Clone)]
pub struct Premise {
    pub kind: PremiseKind,
    pub paren: Paren,
    pub commit_point: Option<Token![!]>,
}

#[derive(Debug, Clone)]
pub enum PremiseKind {
    IfLet(IfLet),
    If(If),
    ForAll(ForAll),
    Fail(Fail),
    Implication(Implication),
    Let(Let),
    LetForAll(LetForAll),
    In(In),
    // TODO: Define Pub Structure
    Scope(Scope),
}

#[derive(Debug, Clone)]
pub struct IfLet {
    pub pattern: Pat,
    pub expression: Expr,
}

#[derive(Debug, Clone)]
pub struct If {
    pub expression: Expr,
}

#[derive(Debug, Clone)]
pub struct ForAll {
    pub identifier: Ident,
    pub collection: Expr,
    pub state: Vec<Ident>,
    pub body: Premises,
}

#[derive(Debug, Clone)]
pub struct Fail {
    pub message: LitStr,
    pub expressions: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Implication {
    pub hypothesis: Expr,
    pub conclusion: Pat,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub binding: Binding,
    pub expression: Expr,
}

#[derive(Debug, Clone)]
pub struct LetForAll {
    pub binding: Binding,
    pub loop_var: Ident,
    pub expression: Expr,
    pub body: Premises,
}

#[derive(Debug, Clone)]
pub struct In {
    pub pattern: Pat,
    pub expression: Expr,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub expression: Expr,
    pub pattern: Pat,
    pub state: Vec<Ident>,
    pub body: Premises,
}

#[derive(Debug, Clone)]
pub enum Binding {
    Pattern { pattern: Pat },
    Identifier { identifier: Ident, ty: Option<Type> },
}

/// Conclusion ::= Identifier "(" { Pattern } ")" "=>" Expression
#[derive(Debug, Clone)]
pub struct Conclusion {
    pub name: Ident,
    pub patterns: Vec<Pat>,
    pub result: Expr,
}

impl Judgment {
    pub fn name(&self) -> &Ident {
        &self.signature.name
    }

    pub fn input_tys(&self) -> Vec<&Type> {
        self.signature
            .arguments
            .iter()
            .map(|Argument { name, ty }| ty)
            .collect()
    }

    pub fn input_names(&self) -> Vec<&Ident> {
        self.signature
            .arguments
            .iter()
            .map(|Argument { name, ty }| name)
            .collect()
    }

    pub fn output(&self) -> &Type {
        &self.signature.return_
    }

    pub fn debug_input_names(&self) -> &Vec<Ident> {
        &self.body.debug.idents
    }

    pub fn assertions(&self) -> Vec<&Expr> {
        self.body
            .asserts
            .assertions
            .iter()
            .map(|Assert { assertion }| assertion)
            .collect()
    }

    pub fn trivials_exprs(&self) -> Vec<&Expr> {
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

    pub fn trivials_results(&self) -> Vec<&Expr> {
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

    pub fn rules(&self) -> Vec<&Rule> {
        self.body.rules.rules.iter().collect()
    }
}
