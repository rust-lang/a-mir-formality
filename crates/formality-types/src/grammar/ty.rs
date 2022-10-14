use formality_core::interned::{Internable, Interned, Interner};
use formality_macros::Fold;

use crate::{collections::Map, fold::Fold};

use super::{AdtId, AssociatedItemId, Binder, FnId, Predicate, TraitId};

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

#[derive(Fold, Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Universe {
    pub index: usize,
}

impl Universe {
    /// The root universe contains only the names globally visible
    /// (e.g., structs defined by user) and does not contain any [placeholders](`PlaceholderVar`).
    pub const ROOT: Universe = Universe { index: 0 };
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

impl Internable for TyData {
    fn table() -> &'static Interner<Self> {
        lazy_static::lazy_static! {
            static ref INTERNER: Interner<TyData> = Interner::default();
        }
        &*INTERNER
    }
}

impl<T> From<T> for Ty
where
    T: Into<TyData>,
{
    fn from(v: T) -> Ty {
        let v: TyData = v.into();
        Ty {
            data: Interned::new(v),
        }
    }
}

// NB: TyData doesn't implement Fold; you fold types, not TyData,
// because variables might not map to the same variant.
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InferenceVar {
    index: usize,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RigidTy {
    name: RigidName,
    parameters: Parameters,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RigidName {
    Adt(AdtId),
    ScalarId(ScalarId),
    Ref(RefKind),
    Tuple(usize),
    FnPtr(usize),
    FnDef(FnId),
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RefKind {
    Shared,
    Mut,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AliasTy {
    name: AliasName,
    parameters: Parameters,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AliasName {
    AssociatedTyId(AssociatedTyId),
}

from_impl!(impl From<AssociatedTyId> for AliasName);

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AssociatedTyId {
    pub trait_id: TraitId,
    pub item_id: AssociatedItemId,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PredicateTy {
    ForAllTy(Binder<Ty>),
    ExistsTy(Binder<Ty>),
    ImplicationTy(ImplicationTy),
    EnsuresTy(EnsuresTy),
}

from_impl!(impl From<ImplicationTy> for PredicateTy);
from_impl!(impl From<EnsuresTy> for PredicateTy);

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplicationTy {
    pub predicates: Vec<Predicate>,
    pub ty: Ty,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnsuresTy {
    ty: Ty,
    predicates: Vec<Predicate>,
}

/// A *placeholder* is a dummy variable about which nothing is known except
/// that which we see in the environment. When we want to prove something
/// is true for all `T`, we replace `T` with a placeholder.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PlaceholderVar {
    universe: Universe,
    index: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QuantifierKind {
    ForAll,
    Exists,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KindedVarIndex {
    pub kind: ParameterKind,
    pub var_index: VarIndex,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Parameter {
    Ty(Ty),
    Lt(Lt),
}

from_impl!(impl From<Ty> for Parameter);
from_impl!(impl From<Lt> for Parameter);

pub type Parameters = Vec<Parameter>;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParameterKind {
    Ty,
    Lt,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lt {
    data: Interned<LtData>,
}

impl Lt {
    pub fn data(&self) -> &LtData {
        &self.data
    }
}

impl Internable for LtData {
    fn table() -> &'static Interner<Self> {
        lazy_static::lazy_static! {
            static ref INTERNER: Interner<LtData> = Interner::default();
        }
        &*INTERNER
    }
}

impl<V> From<V> for Lt
where
    V: Into<LtData>,
{
    fn from(v: V) -> Self {
        let data: LtData = v.into();
        Lt {
            data: Interned::from(data),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LtData {
    Static,
    Variable(Variable),
}

from_impl!(impl From<Variable> for LtData);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

    /// Shift a variable in through `binders` binding levels.
    /// Only affects bound variables.
    pub fn shift_in(&self) -> Self {
        if let Variable::BoundVar(BoundVar {
            debruijn: Some(db),
            var_index,
        }) = self
        {
            BoundVar {
                debruijn: Some(db.shift_in()),
                var_index: *var_index,
            }
            .into()
        } else {
            self.clone()
        }
    }

    /// Shift a variable out through `binders` binding levels.
    /// Only affects bound variables. Returns None if the variable
    /// is bound within those binding levels.
    pub fn shift_out(&self) -> Option<Self> {
        if let Variable::BoundVar(BoundVar {
            debruijn: Some(db),
            var_index,
        }) = self
        {
            db.shift_out().map(|db1| {
                BoundVar {
                    debruijn: Some(db1),
                    var_index: *var_index,
                }
                .into()
            })
        } else {
            Some(self.clone())
        }
    }

    /// A variable is *free* (i.e., not bound by any internal binder)
    /// if it is an inference variable, a placeholder, or has a debruijn
    /// index of `None`. The latter occurs when you `open` a binder (and before
    /// you close it back up again).
    pub fn is_free(&self) -> bool {
        match self {
            Variable::PlaceholderVar(_) | Variable::InferenceVar(_) => true,

            Variable::BoundVar(_) => false,
        }
    }
}

/// Identifies a bound variable.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DebruijnIndex {
    pub index: usize,
}

impl DebruijnIndex {
    pub const INNERMOST: DebruijnIndex = DebruijnIndex { index: 0 };

    /// Adjust this debruijn index through a binder level.
    pub fn shift_in(&self) -> Self {
        DebruijnIndex {
            index: self.index + 1,
        }
    }

    /// Adjust this debruijn index *outward* through a binder level, if possible.
    pub fn shift_out(&self) -> Option<Self> {
        if self.index > 0 {
            Some(DebruijnIndex {
                index: self.index - 1,
            })
        } else {
            None
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarIndex {
    pub index: u64,
}

#[derive(Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Substitution {
    map: Map<Variable, Parameter>,
}

impl Extend<(Variable, Parameter)> for Substitution {
    fn extend<T: IntoIterator<Item = (Variable, Parameter)>>(&mut self, iter: T) {
        self.map.extend(iter.into_iter().map(|(v, p)| {
            assert!(v.is_free());
            (v, p)
        }));
    }
}

impl FromIterator<(Variable, Parameter)> for Substitution {
    fn from_iter<T: IntoIterator<Item = (Variable, Parameter)>>(iter: T) -> Self {
        let mut s = Substitution::default();
        s.extend(iter);
        s
    }
}

impl Substitution {
    pub fn apply<T: Fold>(&self, t: &T) -> T {
        t.substitute(&mut |_kind, v| self.map.get(v).cloned())
    }
}
