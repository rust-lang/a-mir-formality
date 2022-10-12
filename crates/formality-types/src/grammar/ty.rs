use formality_core::interned::{Interned, Interner};

use super::{AdtId, AssociatedItemId, FnId, Predicate, TraitId, VarId};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Universe {
    pub index: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ty {
    data: Interned<TyData>,
}

impl Ty {
    pub fn data(&self) -> &TyData {
        &self.data
    }
}

pub fn Ty(data: &TyData) -> Ty {
    lazy_static::lazy_static! {
        static ref INTERNER: Interner<TyData> = Interner::default();
    }
    Ty {
        data: INTERNER.intern(data),
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TyData {
    RigidTy(RigidTy),
    AliasTy(AliasTy),
    PredicateTy(PredicateTy),
    Variable(Variable),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InferenceVar {
    index: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RigidTy {
    name: RigidName,
    parameters: Parameters,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RigidName {
    Adt(AdtId),
    ScalarId(ScalarId),
    Ref(RefKind),
    Tuple(usize),
    FnPtr(usize),
    FnDef(FnId),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RefKind {
    Shared,
    Mut,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScalarId {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    Bool,
    USize,
    ISize,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AliasTy {
    name: AliasName,
    parameters: Parameters,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AliasName {
    AssociatedTyId(AssociatedTyId),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AssociatedTyId {
    pub trait_id: TraitId,
    pub item_id: AssociatedItemId,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PredicateTy {
    ForAllTy(Binder<Ty>),
    ExistsTy(Binder<Ty>),
    ImplicationTy(ImplicationTy),
    EnsuresTy(EnsuresTy),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplicationTy {
    pub predicates: Vec<Predicate>,
    pub ty: Ty,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnsuresTy {
    ty: Ty,
    predicates: Vec<Predicate>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PlaceholderVar {
    universe: Universe,
    index: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QuantifierKind {
    ForAll,
    Exists,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Binder<T> {
    pub variables: Vec<KindedVarId>,
    pub data: T,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KindedVarId {
    pub kind: ParameterKind,
    pub id: VarId,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Parameter {
    Ty(Ty),
    Lt(Lt),
}

pub type Parameters = Vec<Parameter>;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParameterKind {
    Ty,
    Lt,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lt {
    data: Interned<LtData>,
}

pub fn Lt(data: &LtData) -> Lt {
    lazy_static::lazy_static! {
        static ref INTERNER: Interner<LtData> = Interner::default();
    }
    Lt {
        data: INTERNER.intern(data),
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LtData {
    Static,
    Variable(Variable),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Variable {
    PlaceholderVar(PlaceholderVar),
    InferenceVar(InferenceVar),
    BoundVar(VarId),
}
