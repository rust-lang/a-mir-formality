use std::sync::Arc;

use crate::grammar::Fn;
use crate::grammar::{
    AliasTy, AssociatedItemId, Binder, Const, Fallible, Lt, Parameter, ParameterKind, Predicate,
    TraitId, TraitRef, Ty, Wc, Wcs,
};
use crate::prove::Safety;
use crate::rust::Term;
use formality_core::{term, Upcast};

#[term($?safety $?applicability trait $id $binder)]
pub struct Trait {
    pub safety: Safety,
    pub applicability: Applicability,
    pub id: TraitId,
    pub binder: TraitBinder<TraitBoundData>,
}

/// `always_applicable trait Foo`: every impl of `Foo` must be *always
/// applicable* (rustc's `#[rustc_specialization_trait]`): whether it applies
/// to a type does not depend on the type's lifetimes. A bound on such a
/// trait can then be decided after lifetime erasure, so `if impls T: Foo`
/// needs no `may_spec`. Requires `#![feature(spec_always_applicable)]`.
#[term]
#[derive(Default)]
pub enum Applicability {
    #[default]
    Any,
    #[grammar(always_applicable)]
    Always,
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

    /// `may_spec(T: Trait)`: the function needs to know whether the bound
    /// holds, and its callers must decide it (see [`Predicate::MaySpec`]).
    /// Requires `#![feature(branch_specialization)]`.
    #[grammar(may_spec($v0))]
    MaySpec(MaySpecBound),
}

/// The bound a `may_spec` where-clause or an `if impls` names: a trait
/// bound, possibly under `for<..>`.
#[term]
pub enum MaySpecBound {
    #[grammar($v0 : $v1 $<?v2>)]
    IsImplemented(Ty, TraitId, Vec<Parameter>),

    #[grammar(for $v0)]
    ForAll(Arc<Binder<MaySpecBound>>),
}

impl MaySpecBound {
    /// The `Wc` this bound stands for.
    pub fn to_wc(&self) -> Wc {
        match self {
            MaySpecBound::IsImplemented(self_ty, trait_id, parameters) => {
                Predicate::is_implemented(trait_id.with(self_ty, parameters)).upcast()
            }
            MaySpecBound::ForAll(binder) => {
                let (vars, bound) = binder.open();
                Wc::for_all(Binder::new(&vars, bound.to_wc()))
            }
        }
    }

    /// `may_spec(WC)` asserts nothing about `WC` (it may hold or not), so only
    /// the parameters of `WC` need be well-formed; in particular the trait's
    /// where-clauses (its supertraits) are not required to hold.
    pub fn well_formed(&self) -> Wcs {
        match self {
            MaySpecBound::IsImplemented(self_ty, _trait_id, parameters) => {
                std::iter::once(Predicate::well_formed(self_ty))
                    .chain(parameters.iter().map(Predicate::well_formed))
                    .collect()
            }
            MaySpecBound::ForAll(binder) => {
                let (vars, bound) = binder.open();
                bound
                    .well_formed()
                    .into_iter()
                    .map(|wc| Wc::for_all(Binder::new(&vars, wc)))
                    .collect()
            }
        }
    }

    pub fn has_non_lifetime_binder(&self) -> bool {
        match self {
            MaySpecBound::ForAll(binder) => {
                binder
                    .kinds()
                    .iter()
                    .any(|kind| !matches!(kind, ParameterKind::Lt))
                    || binder.peek().has_non_lifetime_binder()
            }
            MaySpecBound::IsImplemented(..) => false,
        }
    }
}

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
            // Not a fact about the bound, so it has no inverse.
            WhereClause::MaySpec(_) => None,
        }
    }

    pub fn well_formed(&self) -> Wcs {
        match self {
            WhereClause::IsImplemented(self_ty, trait_id, parameters) => {
                Predicate::well_formed_trait_ref(trait_id.with(self_ty, parameters)).upcast()
            }
            WhereClause::AliasEq(alias_ty, ty) => {
                [Predicate::well_formed(alias_ty), Predicate::well_formed(ty)]
                    .into_iter()
                    .collect()
            }
            WhereClause::Outlives(a, b) => [Predicate::well_formed(a), Predicate::well_formed(b)]
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
                [Predicate::well_formed(ct), Predicate::well_formed(ty)]
                    .into_iter()
                    .collect()
            }
            WhereClause::MaySpec(bound) => bound.well_formed(),
        }
    }

    pub fn has_non_lifetime_binder(&self) -> bool {
        match self {
            WhereClause::ForAll(binder) => {
                binder
                    .kinds()
                    .iter()
                    .any(|kind| !matches!(kind, ParameterKind::Lt))
                    || binder.peek().has_non_lifetime_binder()
            }
            WhereClause::MaySpec(bound) => bound.has_non_lifetime_binder(),
            _ => false,
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
