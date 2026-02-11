use std::sync::Arc;

use crate::grammar::Fn;
use crate::grammar::{
    AliasTy, AssociatedItemId, Binder, Const, Fallible, Lt, Parameter, TraitId, TraitRef, Ty, Wc,
    Wcs,
};
use crate::prove::prove::Safety;
use crate::rust::Term;
use formality_core::{term, Upcast};

#[term($?safety trait $id $binder)]
pub struct Trait {
    pub safety: Safety,
    pub id: TraitId,
    pub binder: TraitBinder<TraitBoundData>,
}

// NB: TraitBinder is a manually implemented Term
// that binds the `Self` variable.
#[derive(Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct TraitBinder<T: Term> {
    pub explicit_binder: Binder<T>,
}

impl<T: Term> TraitBinder<T> {
    pub fn instantiate_with(&self, parameters: &[impl Upcast<Parameter>]) -> Fallible<T> {
        self.explicit_binder.instantiate_with(parameters)
    }
}

#[term($:where $,where_clauses { $*trait_items })]
pub struct TraitBoundData {
    pub where_clauses: Vec<WhereClause>,
    pub trait_items: Vec<TraitItem>,
}

#[term]
pub enum TraitItem {
    #[cast]
    Fn(Fn),
    #[cast]
    AssociatedTy(AssociatedTy),
}
#[term(type $id $binder ;)]
pub struct AssociatedTy {
    pub id: AssociatedItemId,
    pub binder: Binder<AssociatedTyBoundData>,
}

#[term(: $ensures $:where $,where_clauses)]
pub struct AssociatedTyBoundData {
    /// So e.g. `type Item : [Sized]` would be encoded as `<type I> (I: Sized)`.
    pub ensures: Vec<WhereBound>,

    /// Where clauses that must hold.
    pub where_clauses: Vec<WhereClause>,
}

#[term($?safety impl $binder)]
pub struct TraitImpl {
    pub safety: Safety,
    pub binder: Binder<TraitImplBoundData>,
}

impl TraitImpl {
    pub fn trait_id(&self) -> &TraitId {
        &self.binder.peek().trait_id
    }
}

#[term($trait_id $<?trait_parameters> for $self_ty $:where $,where_clauses { $*impl_items })]
pub struct TraitImplBoundData {
    pub trait_id: TraitId,
    pub self_ty: Ty,
    pub trait_parameters: Vec<Parameter>,
    pub where_clauses: Vec<WhereClause>,
    pub impl_items: Vec<ImplItem>,
}

impl TraitImplBoundData {
    pub fn trait_ref(&self) -> TraitRef {
        self.trait_id.with(&self.self_ty, &self.trait_parameters)
    }
}

#[term($?safety impl $binder)]
pub struct NegTraitImpl {
    pub safety: Safety,
    pub binder: Binder<NegTraitImplBoundData>,
}

#[term(!$trait_id $<?trait_parameters> for $self_ty $:where $,where_clauses { })]
pub struct NegTraitImplBoundData {
    pub trait_id: TraitId,
    pub self_ty: Ty,
    pub trait_parameters: Vec<Parameter>,
    pub where_clauses: Vec<WhereClause>,
}

impl NegTraitImplBoundData {
    pub fn trait_ref(&self) -> TraitRef {
        self.trait_id.with(&self.self_ty, &self.trait_parameters)
    }
}

#[term]
pub enum ImplItem {
    #[cast]
    Fn(Fn),
    #[cast]
    AssociatedTyValue(AssociatedTyValue),
}

#[term(type $id $binder ;)]
pub struct AssociatedTyValue {
    pub id: AssociatedItemId,
    pub binder: Binder<AssociatedTyValueBoundData>,
}

#[term(= $ty $:where $,where_clauses)]
pub struct AssociatedTyValueBoundData {
    pub where_clauses: Vec<WhereClause>,
    pub ty: Ty,
}

#[term($data)]
pub struct WhereClause {
    pub data: Arc<WhereClauseData>,
}

impl WhereClause {
    pub fn data(&self) -> &WhereClauseData {
        &self.data
    }

    pub fn invert(&self) -> Option<Wc> {
        match self.data() {
            WhereClauseData::IsImplemented(self_ty, trait_id, parameters) => Some(
                trait_id
                    .with(self_ty, parameters)
                    .not_implemented()
                    .upcast(),
            ),
            WhereClauseData::AliasEq(_, _) => None,
            WhereClauseData::Outlives(_, _) => None,
            WhereClauseData::ForAll(binder) => {
                let (vars, where_clause) = binder.open();
                let wc = where_clause.invert()?;
                Some(Wc::for_all(&vars, wc))
            }
            WhereClauseData::TypeOfConst(_, _) => None,
        }
    }

    pub fn well_formed(&self) -> Wcs {
        match self.data() {
            WhereClauseData::IsImplemented(self_ty, trait_id, parameters) => {
                trait_id.with(self_ty, parameters).well_formed().upcast()
            }
            WhereClauseData::AliasEq(alias_ty, ty) => {
                let alias_param: Parameter = alias_ty.upcast();
                let ty_param: Parameter = ty.upcast();
                [
                    alias_param.well_formed().upcast(),
                    ty_param.well_formed().upcast(),
                ]
                .into_iter()
                .collect()
            }
            WhereClauseData::Outlives(a, b) => {
                let a_param: Parameter = a.upcast();
                let b_param: Parameter = b.upcast();
                [
                    a_param.well_formed().upcast(),
                    b_param.well_formed().upcast(),
                ]
                .into_iter()
                .collect()
            }
            WhereClauseData::ForAll(binder) => {
                let (vars, body) = binder.open();
                body.well_formed()
                    .into_iter()
                    .map(|wc| Wc::for_all(&vars, wc))
                    .collect()
            }
            WhereClauseData::TypeOfConst(ct, ty) => {
                let ct_param: Parameter = ct.upcast();
                let ty_param: Parameter = ty.upcast();
                [
                    ct_param.well_formed().upcast(),
                    ty_param.well_formed().upcast(),
                ]
                .into_iter()
                .collect()
            }
        }
    }
}

#[term]
pub enum WhereClauseData {
    #[grammar($v0 : $v1 $<?v2>)]
    IsImplemented(Ty, TraitId, Vec<Parameter>),

    #[grammar($v0 => $v1)]
    AliasEq(AliasTy, Ty),

    #[grammar($v0 : $v1)]
    Outlives(Parameter, Lt),

    #[grammar(for $v0)]
    ForAll(Binder<WhereClause>),

    #[grammar(type_of_const $v0 is $v1)]
    TypeOfConst(Const, Ty),
}

#[term($data)]
pub struct WhereBound {
    pub data: Arc<WhereBoundData>,
}

impl WhereBound {
    pub fn data(&self) -> &WhereBoundData {
        &self.data
    }
}

#[term]
pub enum WhereBoundData {
    #[grammar($v0 $<?v1>)]
    IsImplemented(TraitId, Vec<Parameter>),

    #[grammar($v0)]
    Outlives(Lt),

    #[grammar(for $v0)]
    ForAll(Binder<WhereBound>),
}
