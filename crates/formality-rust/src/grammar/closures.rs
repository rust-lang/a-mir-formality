use crate::grammar::fns::InputArg;
use crate::grammar::minirust::LocalId;
use crate::grammar::{Binder, ClosureId, MaybeFnBody, Ty, WhereClause};
use formality_core::term;

#[term(closure $id $binder)]
pub struct ClosureDef {
    pub id: ClosureId,
    pub binder: Binder<ClosureDefBoundData>,
}

#[term([$,captures] ($,input_args) -> $output_ty $:where $,where_clauses $body)]
pub struct ClosureDefBoundData {
    pub captures: Vec<CaptureDecl>,
    pub input_args: Vec<InputArg>,
    pub output_ty: Ty,
    pub where_clauses: Vec<WhereClause>,
    pub body: MaybeFnBody,
}

#[term($id : $mode $ty)]
pub struct CaptureDecl {
    pub id: LocalId,
    pub mode: CaptureMode,
    pub ty: Ty,
}

#[term]
pub enum CaptureMode {
    #[grammar(by_value)]
    ByValue,
    #[grammar(by_ref)]
    ByRef,
    #[grammar(by_mut_ref)]
    ByMutRef,
}
