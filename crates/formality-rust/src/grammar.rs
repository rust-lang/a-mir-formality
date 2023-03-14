use std::sync::Arc;

use formality_macros::term;
use formality_types::{
    cast::Upcast,
    grammar::{
        AdtId, AssociatedItemId, Binder, CrateId, Fallible, FieldId, FnId, Lt, Parameter, TraitId,
        Ty,
    },
    term::Term,
};

use crate::grammar::mir::MirFnBody;

pub mod mir;

#[term($crates)]
pub struct Program {
    /// List of all crates.
    /// The last crate in the list is the current crate.
    pub crates: Vec<Crate>,
}

impl Program {
    pub fn items_from_all_crates(&self) -> impl Iterator<Item = &CrateItem> {
        self.crates.iter().flat_map(|c| &c.items)
    }

    pub fn trait_named(&self, trait_id: &TraitId) -> Fallible<&Trait> {
        let mut traits: Vec<&Trait> = self
            .items_from_all_crates()
            .filter_map(|crate_item| match crate_item {
                CrateItem::Trait(t) if t.id == *trait_id => Some(t),
                _ => None,
            })
            .collect();
        if traits.len() < 1 {
            anyhow::bail!("no trait named `{trait_id:?}`")
        } else if traits.len() > 1 {
            anyhow::bail!("multiple traits named `{trait_id:?}`")
        } else {
            Ok(traits.pop().unwrap())
        }
    }
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
    #[cast]
    Fn(Fn),
}

#[term(struct $id $binder)]
pub struct Struct {
    pub id: AdtId,
    pub binder: Binder<StructBoundData>,
}

impl Struct {
    pub fn to_adt(&self) -> Adt {
        let (
            vars,
            StructBoundData {
                where_clauses,
                fields,
            },
        ) = self.binder.open();
        Adt {
            id: self.id.clone(),
            binder: Binder::new(
                &vars,
                AdtBoundData {
                    where_clauses: where_clauses,
                    variants: vec![Variant {
                        name: VariantId::for_struct(),
                        fields,
                    }],
                },
            ),
        }
    }
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

#[term]
pub enum FieldName {
    #[cast]
    Id(FieldId),
    #[cast]
    Index(usize),
}

formality_types::id!(VariantId);

impl VariantId {
    /// Returns the special variant-id used for the single variant of a struct.
    pub fn for_struct() -> Self {
        VariantId::new("struct")
    }
}

#[term(enum $id $binder)]
pub struct Enum {
    pub id: AdtId,
    pub binder: Binder<AdtBoundData>,
}

impl Enum {
    pub fn to_adt(&self) -> Adt {
        Adt {
            id: self.id.clone(),
            binder: self.binder.clone(),
        }
    }
}

/// Not directly part of the grammar, but structs/enums
/// can be converted to this.
#[term(adt $id $binder)]
pub struct Adt {
    pub id: AdtId,
    pub binder: Binder<AdtBoundData>,
}

#[term(where $where_clauses { $,variants })]
pub struct AdtBoundData {
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

impl<T: Term> TraitBinder<T> {
    pub fn instantiate_with(&self, parameters: &[impl Upcast<Parameter>]) -> Fallible<T> {
        self.explicit_binder.instantiate_with(parameters)
    }
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

    #[cast]
    #[grammar(= $v0 ;)]
    MirFnBody(MirFnBody),
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
