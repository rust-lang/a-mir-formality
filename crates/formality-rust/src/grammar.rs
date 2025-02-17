use std::sync::Arc;

use formality_core::{term, Upcast};
use formality_prove::Safety;
use formality_types::grammar::Constness;
use formality_types::{
    grammar::{
        AdtId, AliasTy, AssociatedItemId, Binder, Const, ConstData, CrateId, Fallible, FieldId,
        FnId, Lt, Parameter, Relation, TraitId, TraitRef, Ty, Wc, Wcs,
    },
    rust::Term,
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
        if traits.is_empty() {
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
    NegTraitImpl(NegTraitImpl),
    #[cast]
    Fn(Fn),
    #[cast]
    Test(Test),
}

#[term(test $binder)]
pub struct Test {
    pub binder: Binder<TestBoundData>,
}

#[term($:where $,assumptions { $,goals })]
pub struct TestBoundData {
    pub assumptions: Vec<WhereClause>,
    pub goals: Vec<WhereClause>,
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
                vars,
                AdtBoundData {
                    where_clauses,
                    variants: vec![Variant {
                        name: VariantId::for_struct(),
                        fields,
                    }],
                },
            ),
        }
    }
}

#[term($:where $,where_clauses { $,fields })]
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

formality_core::id!(VariantId);

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

#[term($:where $,where_clauses { $,variants })]
pub struct AdtBoundData {
    pub where_clauses: Vec<WhereClause>,
    pub variants: Vec<Variant>,
}

#[term($name { $,fields })]
pub struct Variant {
    pub name: VariantId,
    pub fields: Vec<Field>,
}

#[term($?safety $?constness trait $id $binder)]
pub struct Trait {
    pub safety: Safety,
    pub constness: Constness,
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

pub struct ConstEffects {
    pub effect_items: Vec<ConstEffect>,
}

pub enum ConstEffect {
    Const,
    NotConst,
    // For <T as Trait<..>>::E, TraitRef can uniquely identify an impl, and an impl has only one effect. 
    FullyQualified(TraitRef), }


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

    #[cast]
    #[grammar(= $v0 ;)]
    MirFnBody(MirFnBody),
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
    pub fn constness(&self) -> Constness {
        self.binder.peek().constness.clone()
    }
}

#[term($?constness $trait_id $<?trait_parameters> for $self_ty $:where $,where_clauses { $*impl_items })]
pub struct TraitImplBoundData {
    pub constness: Constness,
    pub trait_id: TraitId,
    pub self_ty: Ty,
    pub trait_parameters: Vec<Parameter>,
    pub where_clauses: Vec<WhereClause>,
    pub impl_items: Vec<ImplItem>,
}

impl TraitImplBoundData {
    pub fn trait_ref(&self) -> TraitRef {
        self.trait_id.with(&self.constness, &self.self_ty, &self.trait_parameters)
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
        self.trait_id.with(&Constness::NotConst, &self.self_ty, &self.trait_parameters)
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
                    .with(&Constness::NotConst, self_ty, parameters)
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
                trait_id.with(&Constness::NotConst, self_ty, parameters).well_formed().upcast()
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
                let mut wcs = vec![];
                match ct.data() {
                    ConstData::Value(_, t) => {
                        wcs.push(Relation::equals(ty, t));
                    }
                    ConstData::Variable(_) => {}
                }
                // FIXME(oli-obk): prove that there is no `TypeOfConst` bound for a different type.
                let ct_param: Parameter = ct.upcast();
                let ty_param: Parameter = ty.upcast();
                wcs.push(ct_param.well_formed());
                wcs.push(ty_param.well_formed());
                wcs.into_iter().map(|r| r.upcast()).collect()
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
