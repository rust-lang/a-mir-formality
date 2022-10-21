use formality_macros::term;
use std::{collections::BTreeSet, sync::Arc};

mod parse_impls;

use crate::{
    cast::{To, Upcast, UpcastFrom},
    collections::Map,
    fold::Fold,
    from_term_impl,
};

use super::{AdtId, AssociatedItemId, Binder, FnId, Predicate, TraitId};

#[term(U($index))]
#[derive(Copy)]
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
}

impl UpcastFrom<TyData> for Ty {
    fn upcast_from(v: TyData) -> Ty {
        Ty { data: Arc::new(v) }
    }
}
from_term_impl!(impl UpcastFrom<RigidTy> for Ty via TyData);
from_term_impl!(impl UpcastFrom<AliasTy> for Ty via TyData);
from_term_impl!(impl UpcastFrom<ScalarId> for Ty via TyData);
from_term_impl!(impl UpcastFrom<PredicateTy> for Ty via TyData);

// NB: TyData doesn't implement Fold; you fold types, not TyData,
// because variables might not map to the same variant.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TyData {
    RigidTy(RigidTy),
    AliasTy(AliasTy),
    PredicateTy(PredicateTy),
    Variable(Variable),
}

impl UpcastFrom<TyData> for TyData {
    fn upcast_from(term: TyData) -> Self {
        term
    }
}

from_term_impl!(impl UpcastFrom<RigidTy> for TyData);
from_term_impl!(impl UpcastFrom<AliasTy> for TyData);
from_term_impl!(impl UpcastFrom<PredicateTy> for TyData);
from_term_impl!(impl UpcastFrom<Variable> for TyData);
from_term_impl!(impl UpcastFrom<PlaceholderVar> for TyData via Variable);
from_term_impl!(impl UpcastFrom<InferenceVar> for TyData via Variable);
from_term_impl!(impl UpcastFrom<BoundVar> for TyData via Variable);
from_term_impl!(impl UpcastFrom<ScalarId> for TyData via RigidTy);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InferenceVar {
    pub index: usize,
}

impl InferenceVar {
    pub fn into_parameter(self, kind: ParameterKind) -> Parameter {
        Variable::InferenceVar(self).into_parameter(kind)
    }
}

#[term((rigid $name $*parameters))]
pub struct RigidTy {
    name: RigidName,
    parameters: Parameters,
}

impl UpcastFrom<ScalarId> for RigidTy {
    fn upcast_from(s: ScalarId) -> Self {
        RigidTy {
            name: s.upcast(),
            parameters: vec![],
        }
    }
}

#[term]
pub enum RigidName {
    #[grammar((adt $v0))]
    AdtId(AdtId),
    #[grammar((scalar $v0))]
    ScalarId(ScalarId),
    #[grammar((& $v0))]
    Ref(RefKind),
    #[grammar((tuple $v0))]
    Tuple(usize),
    #[grammar((fnptr $v0))]
    FnPtr(usize),
    #[grammar((fndef $v0))]
    FnDef(FnId),
}

from_term_impl!(impl UpcastFrom<AdtId> for RigidName);
from_term_impl!(impl UpcastFrom<ScalarId> for RigidName);

#[term]
pub enum RefKind {
    Shared,
    Mut,
}

#[term]
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

#[term((alias $name $*parameters))]
pub struct AliasTy {
    pub name: AliasName,
    pub parameters: Parameters,
}

#[term]
pub enum AliasName {
    AssociatedTyId(AssociatedTyId),
}

from_term_impl!(impl UpcastFrom<AssociatedTyId> for AliasName);

#[term(($trait_id :: $item_id))]
pub struct AssociatedTyId {
    pub trait_id: TraitId,
    pub item_id: AssociatedItemId,
}

#[term]
pub enum PredicateTy {
    #[grammar((forall $v0))]
    ForAllTy(Binder<Ty>),
    #[grammar((exists $v0))]
    ExistsTy(Binder<Ty>),
    ImplicationTy(ImplicationTy),
    EnsuresTy(EnsuresTy),
}

from_term_impl!(impl UpcastFrom<ImplicationTy> for PredicateTy);
from_term_impl!(impl UpcastFrom<EnsuresTy> for PredicateTy);

#[term(($predicates => $ty))]
pub struct ImplicationTy {
    pub predicates: Vec<Predicate>,
    pub ty: Ty,
}

#[term(($ty ensures $predicates))]
pub struct EnsuresTy {
    ty: Ty,
    predicates: Vec<Predicate>,
}

/// A *placeholder* is a dummy variable about which nothing is known except
/// that which we see in the environment. When we want to prove something
/// is true for all `T`, we replace `T` with a placeholder.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PlaceholderVar {
    pub universe: Universe,
    pub var_index: VarIndex,
}

impl PlaceholderVar {
    pub fn into_parameter(self, kind: ParameterKind) -> Parameter {
        Variable::PlaceholderVar(self).into_parameter(kind)
    }
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

impl UpcastFrom<KindedVarIndex> for BoundVar {
    fn upcast_from(term: KindedVarIndex) -> Self {
        BoundVar {
            debruijn: None,
            var_index: term.var_index,
        }
    }
}

impl UpcastFrom<KindedVarIndex> for Variable {
    fn upcast_from(term: KindedVarIndex) -> Self {
        term.to::<BoundVar>().to()
    }
}

impl UpcastFrom<KindedVarIndex> for Parameter {
    fn upcast_from(term: KindedVarIndex) -> Self {
        term.to::<BoundVar>().into_parameter(term.kind)
    }
}

#[term]
pub enum Parameter {
    Ty(Ty),
    Lt(Lt),
}

impl Parameter {
    pub fn kind(&self) -> ParameterKind {
        match self {
            Parameter::Ty(_) => ParameterKind::Ty,
            Parameter::Lt(_) => ParameterKind::Lt,
        }
    }

    pub fn as_variable(&self) -> Option<Variable> {
        match self {
            Parameter::Ty(v) => v.as_variable(),
            Parameter::Lt(v) => v.as_variable(),
        }
    }
}

from_term_impl!(impl UpcastFrom<Ty> for Parameter);
from_term_impl!(impl UpcastFrom<Lt> for Parameter);

pub type Parameters = Vec<Parameter>;

#[term]
#[derive(Copy)]
pub enum ParameterKind {
    Ty,
    Lt,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
            LtData::Variable(v) => Some(v.clone()),
            _ => None,
        }
    }
}

impl UpcastFrom<LtData> for Lt {
    fn upcast_from(v: LtData) -> Self {
        Lt::new(v)
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

from_term_impl!(impl UpcastFrom<Variable> for LtData);
from_term_impl!(impl UpcastFrom<InferenceVar> for LtData via Variable);
from_term_impl!(impl UpcastFrom<PlaceholderVar> for LtData via Variable);
from_term_impl!(impl UpcastFrom<BoundVar> for LtData via Variable);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Variable {
    PlaceholderVar(PlaceholderVar),
    InferenceVar(InferenceVar),
    BoundVar(BoundVar),
}

from_term_impl!(impl UpcastFrom<PlaceholderVar> for Variable);
from_term_impl!(impl UpcastFrom<InferenceVar> for Variable);
from_term_impl!(impl UpcastFrom<BoundVar> for Variable);

impl Variable {
    pub fn into_parameter(self, kind: ParameterKind) -> Parameter {
        match kind {
            ParameterKind::Lt => Lt::new(self).upcast(),
            ParameterKind::Ty => Ty::new(self).upcast(),
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
            .upcast()
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
                .upcast()
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
        Variable::upcast_from(self).into_parameter(kind)
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
    pub index: usize,
}

#[derive(Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Substitution {
    map: Map<Variable, Parameter>,
}

impl Substitution {
    /// Returns the variables that will be substituted for a new value.
    pub fn domain(&self) -> BTreeSet<Variable> {
        self.map.keys().cloned().collect()
    }
}

impl Extend<(Variable, Parameter)> for Substitution {
    fn extend<T: IntoIterator<Item = (Variable, Parameter)>>(&mut self, iter: T) {
        self.map.extend(iter.into_iter().map(|(v, p)| (v, p)));
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
