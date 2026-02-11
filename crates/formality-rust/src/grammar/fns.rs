use crate::grammar::{minirust, WhereClause};
use crate::grammar::{Binder, FnId, ScalarId, Ty};
use formality_core::term;

#[term(fn $id $binder)]
pub struct Fn {
    pub id: FnId,
    pub binder: Binder<FnBoundData>,
}

#[term($(input_tys) -> $output_ty $:where $,where_clauses $body)]
pub struct FnBoundData {
    pub input_tys: Vec<Ty>,
    pub output_ty: Ty,
    pub where_clauses: Vec<WhereClause>,
    pub body: MaybeFnBody,
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

    #[grammar($v0 _ $v1)]
    Literal(usize, ScalarId),

    #[cast]
    #[grammar(= $v0;)]
    MiniRust(minirust::Body),
}
