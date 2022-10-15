use formality_macros::Fold;
use formality_types::from_impl;
use formality_types::grammar::{
    AdtId, AssociatedTyId, Binder, CrateId, FieldId, FnId, Predicate, TraitId, TraitRef, Ty,
};

pub struct Program {
    /// List of all crates.
    /// The last crate in the list is the current crate.
    crates: Vec<Crate>,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Crate {
    pub id: CrateId,
    pub items: Vec<CrateItem>,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CrateItem {
    Adt(Adt),
    Trait(Trait),
    TraitImpl(TraitImpl),
}

from_impl!(impl From<Adt> for CrateItem);
from_impl!(impl From<Trait> for CrateItem);

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Adt {
    pub kind: AdtKind,
    pub id: AdtId,
    pub binder: Binder<AdtBoundData>,
}

#[derive(Fold, Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AdtKind {
    Struct,
    Enum,
    Union,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AdtBoundData {
    pub where_clauses: Vec<Predicate>,
    pub variants: Vec<AdtVariant>,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AdtVariant {
    pub name: AdtId,
    pub fields: Vec<Field>,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Field {
    pub name: FieldName,
    pub ty: Ty,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FieldName {
    Id(FieldId),
    Index(usize),
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Trait {
    pub id: TraitId,
    pub binder: Binder<TraitBoundData>,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitBoundData {
    pub where_clauses: Vec<Predicate>,
    pub trait_items: Vec<TraitItem>,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TraitItem {
    Fn(Fn),
    AssociatedTy(AssociatedTy),
}

from_impl!(impl From<Fn> for TraitItem);
from_impl!(impl From<AssociatedTy> for TraitItem);

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Fn {
    pub id: FnId,
    pub binder: Binder<FnBoundData>,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FnBoundData {
    pub input_tys: Vec<Ty>,
    pub output_ty: Ty,
    pub where_clauses: Vec<Predicate>,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AssociatedTy {
    pub id: FnId,
    pub binder: Binder<AssociatedTyBoundData>,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AssociatedTyBoundData {
    /// The Binder binds a single variable that is the name for
    /// associated type. So e.g. `type Item: Sized` would be encoded
    /// as `<type I> (I: Sized)`.
    pub ensures: Binder<Vec<Predicate>>,

    /// Where clauses that must hold.
    pub where_clauses: Vec<Predicate>,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitImpl {
    pub binder: Binder<TraitImplBoundData>,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitImplBoundData {
    pub trait_ref: TraitRef,
    pub where_clauses: Vec<Predicate>,
    pub impl_items: Vec<ImplItem>,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ImplItem {
    Fn(Fn),
    AssociatedTyValue(AssociatedTyValue),
}

from_impl!(impl From<Fn> for ImplItem);
from_impl!(impl From<AssociatedTyValue> for ImplItem);

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AssociatedTyValue {
    pub id: AssociatedTyId,
    pub binder: Binder<AssociatedTyValueBoundData>,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AssociatedTyValueBoundData {
    pub where_clauses: Vec<Predicate>,
    pub ty: Ty,
}
