use std::sync::Arc;

use formality_decl::grammar::{FieldName, VariantId};
use formality_macros::term;
use formality_types::{
    grammar::{AdtId, AssociatedItemId, Binder, CrateId, FnId, Lt, Parameter, TraitId, Ty},
    term::Term,
};

#[term($crates)]
pub struct Program {
    /// List of all crates.
    /// The last crate in the list is the current crate.
    pub crates: Vec<Crate>,
}

#[term(crate $id { $*items })]
pub struct Crate {
    pub id: CrateId,
    pub items: Vec<CrateItem>,
}

#[term]
pub enum CrateItem {
    #[cast]
    Struct(Struct),
    #[cast]
    Enum(Enum),
    #[cast]
    Trait(Trait),
    #[cast]
    TraitImpl(TraitImpl),
}

#[term(struct $id $binder)]
pub struct Struct {
    pub id: AdtId,
    pub binder: Binder<StructBoundData>,
}

#[term(where $where_clauses { $,fields })]
pub struct StructBoundData {
    pub where_clauses: Vec<WhereClause>,
    pub fields: Vec<Field>,
}

#[term($name : $ty)]
pub struct Field {
    pub name: FieldName,
    pub ty: Ty,
}

#[term(enum $id $binder)]
pub struct Enum {
    pub id: AdtId,
    pub binder: Binder<EnumBoundData>,
}

#[term(where $where_clauses { $,variants })]
pub struct EnumBoundData {
    pub where_clauses: Vec<WhereClause>,
    pub variants: Vec<Variant>,
}

#[term($name { $,fields })]
pub struct Variant {
    pub name: VariantId,
    pub fields: Vec<Field>,
}

#[term(trait $id $binder)]
pub struct Trait {
    pub id: TraitId,
    pub binder: TraitBinder<TraitBoundData>,
}

// NB: TraitBinder is a manually implemented Term
// that binds the `Self` variable.
#[derive(Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct TraitBinder<T: Term> {
    pub explicit_binder: Binder<T>,
}

#[term(where $where_clauses { $*trait_items })]
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

#[term(fn $id $binder)]
pub struct Fn {
    pub id: FnId,
    pub binder: Binder<FnBoundData>,
}

#[term(($,input_tys) -> $output_ty where $where_clauses $body)]
pub struct FnBoundData {
    pub input_tys: Vec<Ty>,
    pub output_ty: Ty,
    pub where_clauses: Vec<WhereClause>,
    pub body: FnBody,
}

#[term]
pub enum FnBody {
    #[grammar(;)]
    NoFnBody,

    #[grammar({trusted})]
    TrustedFnBody,
}

#[term(type $id $binder)]
pub struct AssociatedTy {
    pub id: AssociatedItemId,
    pub binder: Binder<AssociatedTyBoundData>,
}

#[term(: $ensures where $where_clauses)]
pub struct AssociatedTyBoundData {
    /// So e.g. `type Item : [Sized]` would be encoded as `<type I> (I: Sized)`.
    pub ensures: Vec<WhereBound>,

    /// Where clauses that must hold.
    pub where_clauses: Vec<WhereClause>,
}

#[term(impl $binder)]
pub struct TraitImpl {
    pub binder: Binder<TraitImplBoundData>,
}

#[term($trait_id < $,trait_parameters > for $self_ty where $where_clauses { $*impl_items })]
pub struct TraitImplBoundData {
    pub trait_id: TraitId,
    pub self_ty: Ty,
    pub trait_parameters: Vec<Parameter>,
    pub where_clauses: Vec<WhereClause>,
    pub impl_items: Vec<ImplItem>,
}

#[term]
pub enum ImplItem {
    #[cast]
    Fn(Fn),
    #[cast]
    AssociatedTyValue(AssociatedTyValue),
}

#[term(type $id $binder)]
pub struct AssociatedTyValue {
    pub id: AssociatedItemId,
    pub binder: Binder<AssociatedTyValueBoundData>,
}

#[term(where $where_clauses = $ty)]
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
}

#[term]
pub enum WhereClauseData {
    #[grammar($v0 : $v1 < $,v2 >)]
    IsImplemented(Ty, TraitId, Vec<Parameter>),

    #[grammar($v0 : $v1)]
    Outlives(Parameter, Lt),

    #[grammar(for $v0)]
    ForAll(Binder<WhereClause>),
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
    #[grammar($v0 < $,v1 >)]
    IsImplemented(TraitId, Vec<Parameter>),

    #[grammar($v0)]
    Outlives(Lt),

    #[grammar(for $v0)]
    ForAll(Binder<WhereBound>),
}
