use crate::grammar::expr::Block;
use crate::grammar::{Binder, Ty, ValueId, WhereClause};
use crate::prove::prove::Safety;
use formality_core::term;

#[term($?safety fn $id $binder)]
pub struct Fn {
    pub id: ValueId,
    pub safety: Safety,
    pub binder: Binder<FnBoundData>,
}

#[term($(input_args) $output_ty $:where $,where_clauses $body)]
pub struct FnBoundData {
    pub input_args: Vec<InputArg>,
    pub output_ty: MaybeReturnType,
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

#[term]
pub enum MaybeReturnType {
    #[grammar()]
    Empty,

    #[grammar(-> $v0)]
    Specified(Ty)
}

impl From<MaybeReturnType> for Ty {
    fn from(value: MaybeReturnType) -> Self {
        match value {
            MaybeReturnType::Specified(ty) => ty,
            MaybeReturnType::Empty => Ty::unit(),
        }
    }
}

impl From<&MaybeReturnType> for Ty {
    fn from(value: &MaybeReturnType) -> Self {
        match value {
            MaybeReturnType::Specified(ty) => ty.clone(),
            MaybeReturnType::Empty => Ty::unit(),
        }
    }
}