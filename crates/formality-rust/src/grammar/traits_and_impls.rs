use std::sync::Arc;

use crate::grammar::{
    AliasTy, AssociatedItemId, Binder, Const, Fallible, Lt, Parameter, Predicate, TraitId,
    TraitRef, Ty, Wc, Wcs,
};
use crate::grammar::{Fn, Relation};
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

#[term]
pub enum WhereClause {
    #[grammar($v0 : $v1 $<?v2>)]
    IsImplemented(Ty, TraitId, Vec<Parameter>),

    #[grammar($v0 => $v1)]
    AliasEq(AliasTy, Ty),

    #[grammar($v0 : $v1)]
    Outlives(Parameter, Lt),

    #[grammar(for $v0)]
    ForAll(Arc<Binder<WhereClause>>),

    #[grammar(type_of_const $v0 is $v1)]
    TypeOfConst(Const, Ty),
}

/// Temporary alias for migration -- allows `WhereClauseData::Variant` to still compile.
pub type WhereClauseData = WhereClause;

impl WhereClause {
    pub fn invert(&self) -> Option<Wc> {
        match self {
            WhereClause::IsImplemented(self_ty, trait_id, parameters) => Some(
                Predicate::not_implemented(trait_id.with(self_ty, parameters)),
            )
            .upcast(),
            WhereClause::AliasEq(_, _) => None,
            WhereClause::Outlives(_, _) => None,
            WhereClause::ForAll(binder) => {
                let (vars, where_clause) = binder.open();
                let wc = where_clause.invert()?;
                Some(Wc::for_all(Binder::new(&vars, wc)))
            }
            WhereClause::TypeOfConst(_, _) => None,
        }
    }

    pub fn well_formed(&self) -> Wcs {
        match self {
            WhereClause::IsImplemented(self_ty, trait_id, parameters) => {
                Predicate::well_formed_trait_ref(trait_id.with(self_ty, parameters)).upcast()
            }
            WhereClause::AliasEq(alias_ty, ty) => {
                [Relation::well_formed(alias_ty), Relation::well_formed(ty)]
                    .into_iter()
                    .collect()
            }
            WhereClause::Outlives(a, b) => [Relation::well_formed(a), Relation::well_formed(b)]
                .into_iter()
                .collect(),
            WhereClause::ForAll(binder) => {
                let (vars, body) = binder.open();
                body.well_formed()
                    .into_iter()
                    .map(|wc| Wc::for_all(Binder::new(&vars, wc)))
                    .collect()
            }
            WhereClause::TypeOfConst(ct, ty) => {
                [Relation::well_formed(ct), Relation::well_formed(ty)]
                    .into_iter()
                    .collect()
            }
        }
    }
}

#[term]
pub enum WhereBound {
    #[grammar($v0 $<?v1>)]
    IsImplemented(TraitId, Vec<Parameter>),

    #[grammar($v0)]
    Outlives(Lt),

    #[grammar(for $v0)]
    ForAll(Arc<Binder<WhereBound>>),
}

/// Temporary alias for migration -- allows `WhereBoundData::Variant` to still compile.
pub type WhereBoundData = WhereBound;
