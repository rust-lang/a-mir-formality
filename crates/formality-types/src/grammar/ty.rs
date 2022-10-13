use std::ops::Sub;

use formality_core::interned::{Interned, Interner};

use crate::{collections::Map, fold::Fold};

use super::{AdtId, AssociatedItemId, FnId, Predicate, TraitId};

macro_rules! from_impl {
    (impl From<$t:ident> for $e:ident) => {
        impl From<$t> for $e {
            fn from(v: $t) -> $e {
                $e::$t(v)
            }
        }
    };

    (impl From<$t:ident> for $e:ident $(via $via:ident)+) => {
        impl From<$t> for $e {
            fn from(v: $t) -> $e {
                $(
                    let v: $via = v.into();
                )+
                v.into()
            }
        }
    };
}

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

impl<T> From<T> for Ty
where
    T: Into<TyData>,
{
    fn from(v: T) -> Ty {
        lazy_static::lazy_static! {
            static ref INTERNER: Interner<TyData> = Interner::default();
        }
        let v: TyData = v.into();
        Ty {
            data: INTERNER.intern(data),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TyData {
    RigidTy(RigidTy),
    AliasTy(AliasTy),
    PredicateTy(PredicateTy),
    Variable(Variable),
}

from_impl!(impl From<RigidTy> for TyData);
from_impl!(impl From<AliasTy> for TyData);
from_impl!(impl From<PredicateTy> for TyData);
from_impl!(impl From<Variable> for TyData);
from_impl!(impl From<PlaceholderVar> for TyData via Variable);
from_impl!(impl From<InferenceVar> for TyData via Variable);
from_impl!(impl From<BoundVar> for TyData via Variable);

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

from_impl!(impl From<AssociatedTyId> for AliasName);

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

from_impl!(impl From<ImplicationTy> for PredicateTy);
from_impl!(impl From<EnsuresTy> for PredicateTy);

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
pub struct KindedVarIndex {
    pub kind: ParameterKind,
    pub var_index: VarIndex,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Parameter {
    Ty(Ty),
    Lt(Lt),
}

from_impl!(impl From<Ty> for Parameter);
from_impl!(impl From<Lt> for Parameter);

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

impl<V> From<V> for Lt
where
    V: Into<LtData>,
{
    fn from(v: V) -> Self {
        let data: LtData = v.into();
        lazy_static::lazy_static! {
            static ref INTERNER: Interner<LtData> = Interner::default();
        }
        Lt {
            data: INTERNER.intern(&data),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LtData {
    Static,
    Variable(Variable),
}

from_impl!(impl From<Variable> for LtData);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Variable {
    PlaceholderVar(PlaceholderVar),
    InferenceVar(InferenceVar),
    BoundVar(BoundVar),
}

from_impl!(impl From<PlaceholderVar> for Variable);
from_impl!(impl From<InferenceVar> for Variable);
from_impl!(impl From<BoundVar> for Variable);

impl Variable {
    pub fn into_parameter(self, kind: ParameterKind) -> Parameter {
        match kind {
            ParameterKind::Lt => Lt::from(self).into(),
            ParameterKind::Ty => Ty::from(self).into(),
        }
    }
}

/// Identifies a bound variable.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoundVar {
    /// Identifies the binder that contained this variable, counting "outwards".
    /// When you create a binder with `Binder::new`,
    /// When you open a Binder, you get back `Bound
    pub debruijn: Option<DebruijnIndex>,
    pub var_index: VarIndex,
}

impl BoundVar {
    pub fn into_parameter(self, kind: ParameterKind) -> Parameter {
        Variable::from(self).into_parameter(kind)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DebruijnIndex {
    pub index: u64,
}

impl DebruijnIndex {
    pub const INNERMOST: DebruijnIndex = DebruijnIndex { index: 0 };
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarIndex {
    pub index: u64,
}

#[derive(Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Substitution {
    pub map: Map<Variable, Parameter>,
}

impl Extend<(Variable, Parameter)> for Substitution {
    fn extend<T: IntoIterator<Item = (Variable, Parameter)>>(&mut self, iter: T) {
        self.map.extend(iter);
    }
}

impl FromIterator<(Variable, Parameter)> for Substitution {
    fn from_iter<T: IntoIterator<Item = (Variable, Parameter)>>(iter: T) -> Self {
        Substitution {
            map: iter.into_iter().collect(),
        }
    }
}

impl Substitution {
    pub fn apply<T: Fold>(&self, t: &T) -> T {}
}
