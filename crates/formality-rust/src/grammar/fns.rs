use crate::grammar::expr::Block;
use crate::grammar::{Binder, Ty, ValueId, WhereClause};
use formality_core::term;

#[term(fn $id $binder)]
pub struct Fn {
    pub id: ValueId,
    pub binder: Binder<FnBoundData>,
}

#[term($(input_args) -> $output_ty $:where $,where_clauses $body)]
pub struct FnBoundData {
    pub input_args: Vec<InputArg>,
    pub output_ty: Ty,
    pub where_clauses: Vec<WhereClause>,
    pub body: MaybeFnBody,
}

#[term($id : $ty)]
pub struct InputArg {
    pub id: ValueId,
    pub ty: Ty,
}

#[term]
pub enum MaybeFnBody {
    #[grammar(;)]
    NoFnBody,

    #[cast]
    FnBody(FnBody),
}

#[term]
pub enum FnBody {
    #[grammar({trusted})]
    TrustedFnBody,

    #[cast]
    Expr(Block),
}
