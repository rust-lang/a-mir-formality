use formality_macros::{term, Fold, Parse};
use formality_types::from_impl;
use formality_types::grammar::{
    AdtId, AssociatedTyId, Binder, CrateId, FieldId, FnId, Predicate, TraitId, TraitRef, Ty,
};

#[term($*crates)]
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
    Adt(Adt),
    Trait(Trait),
    TraitImpl(TraitImpl),
}

from_impl!(impl From<Adt> for CrateItem);
from_impl!(impl From<Trait> for CrateItem);

#[term($kind $id $binder)]
pub struct Adt {
    pub kind: AdtKind,
    pub id: AdtId,
    pub binder: Binder<AdtBoundData>,
}

#[term]
pub enum AdtKind {
    Struct,
    Enum,
    Union,
}

#[term(where $where_clauses { $*variants })]
pub struct AdtBoundData {
    pub where_clauses: Vec<Predicate>,
    pub variants: Vec<AdtVariant>,
}

#[term($name { $*fields })]
pub struct AdtVariant {
    pub name: AdtId,
    pub fields: Vec<Field>,
}

#[term($name : $ty)]
pub struct Field {
    pub name: FieldName,
    pub ty: Ty,
}

#[term]
pub enum FieldName {
    Id(FieldId),
    Index(usize),
}

#[term(trait $id $binder)]
pub struct Trait {
    pub id: TraitId,
    pub binder: Binder<TraitBoundData>,
}

#[term(where $where_clauses { $*trait_items })]
pub struct TraitBoundData {
    pub where_clauses: Vec<Predicate>,
    pub trait_items: Vec<TraitItem>,
}

#[term]
pub enum TraitItem {
    Fn(Fn),
    AssociatedTy(AssociatedTy),
}

from_impl!(impl From<Fn> for TraitItem);
from_impl!(impl From<AssociatedTy> for TraitItem);

#[term(fn $id $binder)]
pub struct Fn {
    pub id: FnId,
    pub binder: Binder<FnBoundData>,
}

#[term($input_tys -> $output_ty where $where_clauses)]
pub struct FnBoundData {
    pub input_tys: Vec<Ty>,
    pub output_ty: Ty,
    pub where_clauses: Vec<Predicate>,
}

#[term(type $id $binder)]
pub struct AssociatedTy {
    pub id: FnId,
    pub binder: Binder<AssociatedTyBoundData>,
}

#[term(: $ensures where $where_clauses)]
pub struct AssociatedTyBoundData {
    /// The Binder binds a single variable that is the name for
    /// associated type. So e.g. `type Item: Sized` would be encoded
    /// as `<type I> (I: Sized)`.
    pub ensures: Binder<Vec<Predicate>>,

    /// Where clauses that must hold.
    pub where_clauses: Vec<Predicate>,
}

#[term(impl $binder)]
pub struct TraitImpl {
    pub binder: Binder<TraitImplBoundData>,
}

#[term($trait_ref where $where_clauses { $*impl_items })]
pub struct TraitImplBoundData {
    pub trait_ref: TraitRef,
    pub where_clauses: Vec<Predicate>,
    pub impl_items: Vec<ImplItem>,
}

#[term]
pub enum ImplItem {
    Fn(Fn),
    AssociatedTyValue(AssociatedTyValue),
}

from_impl!(impl From<Fn> for ImplItem);
from_impl!(impl From<AssociatedTyValue> for ImplItem);

#[term(type $id $binder)]
pub struct AssociatedTyValue {
    pub id: AssociatedTyId,
    pub binder: Binder<AssociatedTyValueBoundData>,
}

#[term(where $where_clauses = $ty)]
pub struct AssociatedTyValueBoundData {
    pub where_clauses: Vec<Predicate>,
    pub ty: Ty,
}
