use formality_macros::{Fold, Parse};
use formality_types::from_impl;
use formality_types::grammar::{
    AdtId, AssociatedTyId, Binder, CrateId, FieldId, FnId, Predicate, TraitId, TraitRef, Ty,
};

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Program {
    /// List of all crates.
    /// The last crate in the list is the current crate.
    pub crates: Vec<Crate>,
}

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[grammar(crate $id { $*items })]
pub struct Crate {
    pub id: CrateId,
    pub items: Vec<CrateItem>,
}

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CrateItem {
    Adt(Adt),
    Trait(Trait),
    TraitImpl(TraitImpl),
}

from_impl!(impl From<Adt> for CrateItem);
from_impl!(impl From<Trait> for CrateItem);

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[grammar($kind $id $binder)]
pub struct Adt {
    pub kind: AdtKind,
    pub id: AdtId,
    pub binder: Binder<AdtBoundData>,
}

#[derive(Fold, Parse, Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AdtKind {
    Struct,
    Enum,
    Union,
}

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[grammar(where $where_clauses { $*variants })]
pub struct AdtBoundData {
    pub where_clauses: Vec<Predicate>,
    pub variants: Vec<AdtVariant>,
}

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[grammar($name { $*fields })]
pub struct AdtVariant {
    pub name: AdtId,
    pub fields: Vec<Field>,
}

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[grammar($name : $ty)]
pub struct Field {
    pub name: FieldName,
    pub ty: Ty,
}

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FieldName {
    Id(FieldId),
    Index(usize),
}

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[grammar(trait $id $binder)]
pub struct Trait {
    pub id: TraitId,
    pub binder: Binder<TraitBoundData>,
}

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[grammar(where $where_clauses { $*trait_items })]
pub struct TraitBoundData {
    pub where_clauses: Vec<Predicate>,
    pub trait_items: Vec<TraitItem>,
}

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TraitItem {
    Fn(Fn),
    AssociatedTy(AssociatedTy),
}

from_impl!(impl From<Fn> for TraitItem);
from_impl!(impl From<AssociatedTy> for TraitItem);

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[grammar(fn $id $binder)]
pub struct Fn {
    pub id: FnId,
    pub binder: Binder<FnBoundData>,
}

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[grammar($input_tys -> $output_ty where $where_clauses)]
pub struct FnBoundData {
    pub input_tys: Vec<Ty>,
    pub output_ty: Ty,
    pub where_clauses: Vec<Predicate>,
}

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[grammar(type $id $binder)]
pub struct AssociatedTy {
    pub id: FnId,
    pub binder: Binder<AssociatedTyBoundData>,
}

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[grammar(: $ensures where $where_clauses)]
pub struct AssociatedTyBoundData {
    /// The Binder binds a single variable that is the name for
    /// associated type. So e.g. `type Item: Sized` would be encoded
    /// as `<type I> (I: Sized)`.
    pub ensures: Binder<Vec<Predicate>>,

    /// Where clauses that must hold.
    pub where_clauses: Vec<Predicate>,
}

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[grammar(impl $binder)]
pub struct TraitImpl {
    pub binder: Binder<TraitImplBoundData>,
}

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[grammar($trait_ref where $where_clauses { $*impl_items })]
pub struct TraitImplBoundData {
    pub trait_ref: TraitRef,
    pub where_clauses: Vec<Predicate>,
    pub impl_items: Vec<ImplItem>,
}

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ImplItem {
    Fn(Fn),
    AssociatedTyValue(AssociatedTyValue),
}

from_impl!(impl From<Fn> for ImplItem);
from_impl!(impl From<AssociatedTyValue> for ImplItem);

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[grammar(type $id $binder)]
pub struct AssociatedTyValue {
    pub id: AssociatedTyId,
    pub binder: Binder<AssociatedTyValueBoundData>,
}

#[derive(Fold, Parse, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[grammar(where $where_clauses = $ty)]
pub struct AssociatedTyValueBoundData {
    pub where_clauses: Vec<Predicate>,
    pub ty: Ty,
}
