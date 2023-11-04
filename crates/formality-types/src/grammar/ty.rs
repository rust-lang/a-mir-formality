use formality_core::{cast_impl, term};
use std::sync::Arc;

mod debug_impls;
mod parse_impls;
mod term_impls;
use crate::rust::{BoundVar, Variable};
use formality_core::{DowncastTo, To, Upcast, UpcastFrom};

use super::{
    consts::Const, AdtId, AssociatedItemId, Binder, ExistentialVar, FnId, TraitId, UniversalVar,
};

#[term]
#[cast]
pub struct Ty {
    data: Arc<TyData>,
}

impl Ty {
    pub fn new(data: impl Upcast<TyData>) -> Self {
        Ty {
            data: Arc::new(data.upcast()),
        }
    }

    pub fn data(&self) -> &TyData {
        &self.data
    }

    pub fn to_parameter(&self) -> Parameter {
        Parameter::Ty(self.clone())
    }

    pub fn as_variable(&self) -> Option<Variable> {
        match self.data() {
            TyData::Variable(v) => Some(*v),
            _ => None,
        }
    }

    pub fn is_rigid(&self) -> bool {
        matches!(self.data(), TyData::RigidTy(_))
    }

    pub fn rigid(name: impl Upcast<RigidName>, parameters: impl Upcast<Vec<Parameter>>) -> Self {
        RigidTy {
            name: name.upcast(),
            parameters: parameters.upcast(),
        }
        .upcast()
    }

    pub fn alias(name: impl Upcast<AliasName>, parameters: impl Upcast<Vec<Parameter>>) -> Self {
        AliasTy {
            name: name.upcast(),
            parameters: parameters.upcast(),
        }
        .upcast()
    }

    pub fn ref_ty(&self, l: impl Upcast<Lt>) -> Self {
        let l: Lt = l.upcast();
        Self::rigid(
            RefKind::Shared,
            vec![l.to::<Parameter>(), self.to::<Parameter>()],
        )
    }

    pub fn ref_mut_ty(&self, l: impl Upcast<Lt>) -> Self {
        let l: Lt = l.upcast();
        Self::rigid(
            RefKind::Mut,
            vec![l.to::<Parameter>(), self.to::<Parameter>()],
        )
    }

    pub fn bool() -> Ty {
        RigidTy {
            name: RigidName::ScalarId(ScalarId::Bool),
            parameters: vec![],
        }
        .upcast()
    }
}

impl UpcastFrom<TyData> for Ty {
    fn upcast_from(v: TyData) -> Self {
        Ty::new(v)
    }
}

impl DowncastTo<TyData> for Ty {
    fn downcast_to(&self) -> Option<TyData> {
        Some(self.data().clone())
    }
}

// NB: TyData doesn't implement Fold; you fold types, not TyData,
// because variables might not map to the same variant.
#[term]
pub enum TyData {
    #[cast]
    RigidTy(RigidTy),
    #[cast]
    AliasTy(AliasTy),
    #[cast]
    PredicateTy(PredicateTy),
    #[variable]
    #[precedence(1)]
    Variable(Variable),
}

impl UpcastFrom<Ty> for TyData {
    fn upcast_from(term: Ty) -> Self {
        term.data().clone()
    }
}

#[term((rigid $name $*parameters))]
#[customize(parse)]
pub struct RigidTy {
    pub name: RigidName,
    pub parameters: Parameters,
}

impl UpcastFrom<ScalarId> for RigidTy {
    fn upcast_from(s: ScalarId) -> Self {
        RigidTy {
            name: s.upcast(),
            parameters: vec![],
        }
    }
}

impl DowncastTo<ScalarId> for RigidTy {
    fn downcast_to(&self) -> Option<ScalarId> {
        self.name.downcast_to()
    }
}

#[term]
pub enum RigidName {
    #[grammar((adt $v0))]
    #[cast]
    AdtId(AdtId),
    #[grammar((scalar $v0))]
    #[cast]
    ScalarId(ScalarId),
    #[cast]
    #[grammar(&($v0))]
    Ref(RefKind),
    Tuple(usize),
    FnPtr(usize),
    FnDef(FnId),
}

#[term]
pub enum RefKind {
    Shared,
    Mut,
}

#[term]
pub enum ScalarId {
    #[grammar(u8)]
    U8,
    #[grammar(u16)]
    U16,
    #[grammar(u32)]
    U32,
    #[grammar(u64)]
    U64,
    #[grammar(i8)]
    I8,
    #[grammar(i16)]
    I16,
    #[grammar(i32)]
    I32,
    #[grammar(i64)]
    I64,
    #[grammar(bool)]
    Bool,
    #[grammar(usize)]
    Usize,
    #[grammar(isize)]
    Isize,
}

#[term((alias $name $*parameters))]
#[customize(parse)]
pub struct AliasTy {
    pub name: AliasName,
    pub parameters: Parameters,
}

impl AliasTy {
    pub fn associated_ty(
        trait_id: impl Upcast<TraitId>,
        item_id: impl Upcast<AssociatedItemId>,
        parameters: impl Upcast<Vec<Parameter>>,
    ) -> Self {
        AliasTy {
            name: AssociatedTyName {
                trait_id: trait_id.upcast(),
                item_id: item_id.upcast(),
            }
            .upcast(),
            parameters: parameters.upcast(),
        }
    }
}

#[term]
pub enum AliasName {
    #[cast]
    AssociatedTyId(AssociatedTyName),
}

#[term(($trait_id :: $item_id))]
pub struct AssociatedTyName {
    pub trait_id: TraitId,
    pub item_id: AssociatedItemId,
}

#[term]
pub enum PredicateTy {
    ForAll(Binder<Ty>),
}

#[term]
pub enum Parameter {
    #[cast]
    Ty(Ty),
    #[cast]
    Lt(Lt),
    #[grammar(const $v0)]
    Const(Const),
}

impl Parameter {
    pub fn kind(&self) -> ParameterKind {
        match self {
            Parameter::Ty(_) => ParameterKind::Ty,
            Parameter::Lt(_) => ParameterKind::Lt,
            Parameter::Const(_) => ParameterKind::Const,
        }
    }

    pub fn is_variable(&self) -> bool {
        self.as_variable().is_some()
    }

    pub fn as_variable(&self) -> Option<Variable> {
        match self {
            Parameter::Ty(v) => v.as_variable(),
            Parameter::Lt(v) => v.as_variable(),
            Parameter::Const(v) => v.as_variable(),
        }
    }
}

pub type Parameters = Vec<Parameter>;

#[term]
#[derive(Copy)]
pub enum ParameterKind {
    Ty,
    Lt,
    Const,
}

#[term]
#[derive(Copy)]
pub enum Variance {
    #[grammar(+)]
    Covariant,
    #[grammar(-)]
    Contravariant,
    #[grammar(=)]
    Invariant,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lt {
    data: Arc<LtData>,
}

impl Lt {
    pub fn new(data: impl Upcast<LtData>) -> Self {
        Lt {
            data: Arc::new(data.upcast()),
        }
    }

    pub fn data(&self) -> &LtData {
        &self.data
    }

    pub fn as_variable(&self) -> Option<Variable> {
        match self.data() {
            LtData::Variable(v) => Some(*v),
            _ => None,
        }
    }

    pub fn static_() -> Self {
        LtData::Static.upcast()
    }
}

impl UpcastFrom<LtData> for Lt {
    fn upcast_from(v: LtData) -> Self {
        Lt::new(v)
    }
}

impl DowncastTo<LtData> for Lt {
    fn downcast_to(&self) -> Option<LtData> {
        Some(self.data().clone())
    }
}

impl UpcastFrom<LtData> for Parameter {
    fn upcast_from(v: LtData) -> Self {
        Lt::new(v).upcast()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LtData {
    Static,
    Variable(Variable),
}

impl UpcastFrom<LtData> for LtData {
    fn upcast_from(term: LtData) -> Self {
        term
    }
}

impl UpcastFrom<Variable> for Parameter {
    fn upcast_from(v: Variable) -> Parameter {
        match v.kind() {
            ParameterKind::Lt => Lt::new(v).upcast(),
            ParameterKind::Ty => Ty::new(v).upcast(),
            ParameterKind::Const => Const::new(v).upcast(),
        }
    }
}

impl DowncastTo<Variable> for Parameter {
    fn downcast_to(&self) -> Option<Variable> {
        self.as_variable()
    }
}

cast_impl!((RigidTy) <: (TyData) <: (Ty));
cast_impl!((AliasTy) <: (TyData) <: (Ty));
cast_impl!((ScalarId) <: (TyData) <: (Ty));
cast_impl!((PredicateTy) <: (TyData) <: (Ty));
cast_impl!((RigidTy) <: (Ty) <: (Parameter));
cast_impl!((AliasTy) <: (Ty) <: (Parameter));
cast_impl!((ScalarId) <: (Ty) <: (Parameter));
cast_impl!((PredicateTy) <: (Ty) <: (Parameter));
cast_impl!((TyData) <: (Ty) <: (Parameter));
cast_impl!((Variable) <: (TyData) <: (Ty));
cast_impl!((UniversalVar) <: (Variable) <: (TyData));
cast_impl!((ExistentialVar) <: (Variable) <: (TyData));
cast_impl!((BoundVar) <: (Variable) <: (TyData));
cast_impl!((ScalarId) <: (RigidTy) <: (TyData));
cast_impl!((UniversalVar) <: (Variable) <: (Ty));
cast_impl!((ExistentialVar) <: (Variable) <: (Ty));
cast_impl!((BoundVar) <: (Variable) <: (Ty));
cast_impl!((UniversalVar) <: (Variable) <: (Parameter));
cast_impl!((ExistentialVar) <: (Variable) <: (Parameter));
cast_impl!((BoundVar) <: (Variable) <: (Parameter));
cast_impl!(Lt);
cast_impl!(LtData::Variable(Variable));
cast_impl!((ExistentialVar) <: (Variable) <: (LtData));
cast_impl!((UniversalVar) <: (Variable) <: (LtData));
cast_impl!((BoundVar) <: (Variable) <: (LtData));
cast_impl!((UniversalVar) <: (LtData) <: (Lt));
cast_impl!((ExistentialVar) <: (LtData) <: (Lt));
cast_impl!((BoundVar) <: (LtData) <: (Lt));
