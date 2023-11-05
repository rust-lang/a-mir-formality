use formality_core::{cast_impl, language::HasKind, term};
use std::sync::Arc;

pub use crate::eg::grammar::*;

#[cfg(test)]
mod test;

#[term]
#[derive(Copy)]
pub enum Kind {
    #[grammar(type)]
    Ty,
}

#[term]
pub enum Parameter {
    #[cast]
    Ty(Ty),
}

#[term]
pub enum Ty {
    Integer,

    #[cast]
    StructTy(StructTy),

    #[variable]
    Var(Variable),
}

#[term($id $<?parameters>)]
pub struct StructTy {
    pub id: StructId,
    pub parameters: Vec<Parameter>,
}

#[term(struct $id $bound)]
pub struct StructDecl {
    id: StructId,
    bound: Binder<StructBoundData>,
}

#[term({ $,fields })]
pub struct StructBoundData {
    fields: Vec<FieldDecl>,
}

#[term($name : $ty)]
pub struct FieldDecl {
    name: FieldId,
    ty: Ty,
}

#[term(fn $id $bound)]
pub struct FnDecl {
    id: FnId,
    bound: Binder<StructBoundData>,
}

#[term($(fn_parameters) -> $return_ty { $body })]
pub struct FnBoundData {
    pub fn_parameters: Vec<LocalVariableDecl>,
    pub return_ty: Ty,
    pub body: Expr,
}

#[term($id : $ty)]
pub struct LocalVariableDecl {
    pub id: LocalVarId,
    pub ty: Ty,
}

#[term]
pub enum Expr {
    #[cast]
    LocalVar(LocalVarId),

    #[cast]
    IntegerLiteral(usize),

    #[grammar($v0 { $,v1 })]
    StructLiteral(StructTy, Vec<FieldExpr>),

    #[grammar($v0 + $v1)]
    Add(Arc<Expr>, Arc<Expr>),

    #[grammar($v0 - $v1)]
    Sub(Arc<Expr>, Arc<Expr>),

    #[grammar($v0 * $v1)]
    #[precedence(1)]
    Mul(Arc<Expr>, Arc<Expr>),

    #[grammar($v0 / $v1)]
    #[precedence(1)]
    Div(Arc<Expr>, Arc<Expr>),

    #[grammar(let $v0 = $v1 in $v2)]
    LetIn(LocalVarId, Arc<Expr>, Arc<Expr>),
}

#[term($id : $expr)]
pub struct FieldExpr {
    pub id: FieldId,
    pub expr: Expr,
}

formality_core::id!(StructId);
formality_core::id!(FieldId);
formality_core::id!(FnId);
formality_core::id!(LocalVarId);

cast_impl!((Variable) <: (Ty) <: (Parameter));
cast_impl!((BoundVar) <: (Variable) <: (Parameter));
cast_impl!((ExistentialVar) <: (Variable) <: (Parameter));
cast_impl!((UniversalVar) <: (Variable) <: (Parameter));

impl HasKind<crate::FormalityLang> for Parameter {
    fn kind(&self) -> Kind {
        match self {
            Parameter::Ty(_) => Kind::Ty,
        }
    }
}
