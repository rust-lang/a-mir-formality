use contracts::requires;
use formality_macros::{term, Visit};
use std::{collections::BTreeSet, sync::Arc};

mod debug_impls;
mod parse_impls;

use crate::{
    cast::{DowncastTo, To, Upcast, UpcastFrom},
    cast_impl,
    collections::Map,
    derive_links::Visit,
    fold::Fold,
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

    pub fn next(&self) -> Universe {
        Universe {
            index: self.index + 1,
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Visit)]
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

impl UpcastFrom<Ty> for TyData {
    fn upcast_from(term: Ty) -> Self {
        term.data().clone()
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InferenceVar {
    pub kind: ParameterKind,
    pub var_index: VarIndex,
}

#[term((rigid $name $*parameters))]
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
pub struct AliasTy {
    pub name: AliasName,
    pub parameters: Parameters,
}

impl AliasTy {
    pub fn associated_ty(
        trait_id: TraitId,
        item_id: AssociatedItemId,
        parameters: impl Upcast<Vec<Parameter>>,
    ) -> Self {
        AliasTy {
            name: AssociatedTyName { trait_id, item_id }.upcast(),
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
    Exists(Binder<Ty>),
    #[cast]
    ImplicationTy(ImplicationTy),
    #[cast]
    EnsuresTy(EnsuresTy),
}

#[term(implies($predicates, $ty))]
pub struct ImplicationTy {
    pub predicates: Vec<Predicate>,
    pub ty: Ty,
}

#[term(ensures($ty, $predicates))]
pub struct EnsuresTy {
    pub ty: Ty,
    pub predicates: Vec<Predicate>,
}

/// A *placeholder* is a dummy variable about which nothing is known except
/// that which we see in the environment. When we want to prove something
/// is true for all `T` (`∀T`), we replace `T` with a placeholder.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PlaceholderVar {
    pub kind: ParameterKind,
    pub universe: Universe,
    pub var_index: VarIndex,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QuantifierKind {
    ForAll,
    Exists,
}

#[term]
pub enum Parameter {
    #[cast]
    Ty(Ty),
    #[cast]
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

    pub fn data(&self) -> ParameterData<'_> {
        match self {
            Parameter::Ty(v) => ParameterData::Ty(v.data()),
            Parameter::Lt(v) => ParameterData::Lt(v.data()),
        }
    }
}

pub type Parameters = Vec<Parameter>;

pub enum ParameterData<'me> {
    Ty(&'me TyData),
    Lt(&'me LtData),
}

#[term]
#[derive(Copy)]
pub enum ParameterKind {
    Ty,
    Lt,
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
            LtData::Variable(v) => Some(v.clone()),
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

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Variable {
    PlaceholderVar(PlaceholderVar),
    InferenceVar(InferenceVar),
    BoundVar(BoundVar),
}

cast_impl!(Variable);

impl Variable {
    pub fn kind(&self) -> ParameterKind {
        match self {
            Variable::PlaceholderVar(v) => v.kind,
            Variable::InferenceVar(v) => v.kind,
            Variable::BoundVar(v) => v.kind,
        }
    }

    /// Shift a variable in through `binders` binding levels.
    /// Only affects bound variables.
    pub fn shift_in(&self) -> Self {
        if let Variable::BoundVar(BoundVar {
            debruijn: Some(db),
            var_index,
            kind,
        }) = self
        {
            BoundVar {
                debruijn: Some(db.shift_in()),
                var_index: *var_index,
                kind: *kind,
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
            kind,
        }) = self
        {
            db.shift_out().map(|db1| {
                BoundVar {
                    debruijn: Some(db1),
                    var_index: *var_index,
                    kind: *kind,
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
            Variable::PlaceholderVar(_)
            | Variable::InferenceVar(_)
            | Variable::BoundVar(BoundVar {
                debruijn: None,
                var_index: _,
                kind: _,
            }) => true,

            Variable::BoundVar(BoundVar {
                debruijn: Some(_),
                var_index: _,
                kind: _,
            }) => false,
        }
    }
}

impl Visit for Variable {
    fn free_variables(&self) -> Vec<Variable> {
        if self.is_free() {
            vec![*self]
        } else {
            vec![]
        }
    }
}

impl UpcastFrom<Variable> for Parameter {
    fn upcast_from(v: Variable) -> Parameter {
        match v.kind() {
            ParameterKind::Lt => Lt::new(v).upcast(),
            ParameterKind::Ty => Ty::new(v).upcast(),
        }
    }
}

impl DowncastTo<Variable> for Parameter {
    fn downcast_to(&self) -> Option<Variable> {
        self.as_variable()
    }
}

cast_impl!(BoundVar);
cast_impl!((InferenceVar) <: (Variable) <: (Parameter));
cast_impl!((BoundVar) <: (Variable) <: (Parameter));
cast_impl!((PlaceholderVar) <: (Variable) <: (Parameter));

/// Identifies a bound variable.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoundVar {
    /// Identifies the binder that contained this variable, counting "outwards".
    /// When you create a binder with `Binder::new`,
    /// When you open a Binder, you get back `Bound
    pub debruijn: Option<DebruijnIndex>,
    pub var_index: VarIndex,
    pub kind: ParameterKind,
}

impl BoundVar {
    /// Packages up this bound variable as a type.
    /// Only appropriate to call this if the variable
    /// does indeed represent a type.
    #[requires(self.kind == ParameterKind::Ty)]
    pub fn ty(self) -> Ty {
        Ty::new(TyData::Variable(Variable::upcast_from(self)))
    }

    /// Packages up this bound variable as a lifetime.
    /// Only appropriate to call this if the variable
    /// does indeed represent a lifetime.
    #[requires(self.kind == ParameterKind::Lt)]
    pub fn lt(self) -> Lt {
        Lt::new(LtData::Variable(Variable::upcast_from(self)))
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarIndex {
    pub index: usize,
}

impl VarIndex {
    pub const ZERO: VarIndex = VarIndex { index: 0 };
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
        t.substitute(&mut |v| self.map.get(&v).cloned())
    }
}

/// A substitution that is only between variables.
/// These are reversible.
#[derive(Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarSubstitution {
    map: Map<Variable, Variable>,
}

impl Extend<(Variable, Variable)> for VarSubstitution {
    fn extend<T: IntoIterator<Item = (Variable, Variable)>>(&mut self, iter: T) {
        self.map.extend(iter.into_iter().map(|(v, p)| (v, p)));
    }
}

impl FromIterator<(Variable, Variable)> for VarSubstitution {
    fn from_iter<T: IntoIterator<Item = (Variable, Variable)>>(iter: T) -> Self {
        let mut s = VarSubstitution::default();
        s.extend(iter);
        s
    }
}

impl VarSubstitution {
    pub fn reverse(&self) -> VarSubstitution {
        self.map.iter().map(|(k, v)| (*v, *k)).collect()
    }
    pub fn apply<T: Fold>(&self, t: &T) -> T {
        t.substitute(&mut |v| Some(self.map.get(&v)?.upcast()))
    }
}

cast_impl!(Ty);
cast_impl!((RigidTy) <: (TyData) <: (Ty));
cast_impl!((AliasTy) <: (TyData) <: (Ty));
cast_impl!((ScalarId) <: (TyData) <: (Ty));
cast_impl!((PredicateTy) <: (TyData) <: (Ty));
cast_impl!((RigidTy) <: (Ty) <: (Parameter));
cast_impl!((AliasTy) <: (Ty) <: (Parameter));
cast_impl!((ScalarId) <: (Ty) <: (Parameter));
cast_impl!((PredicateTy) <: (Ty) <: (Parameter));
cast_impl!((TyData) <: (Ty) <: (Parameter));
cast_impl!(TyData::RigidTy(RigidTy));
cast_impl!(TyData::AliasTy(AliasTy));
cast_impl!(TyData::PredicateTy(PredicateTy));
cast_impl!(TyData::Variable(Variable));
cast_impl!((Variable) <: (TyData) <: (Ty));
cast_impl!((PlaceholderVar) <: (Variable) <: (TyData));
cast_impl!((InferenceVar) <: (Variable) <: (TyData));
cast_impl!((BoundVar) <: (Variable) <: (TyData));
cast_impl!((ScalarId) <: (RigidTy) <: (TyData));
cast_impl!((PlaceholderVar) <: (Variable) <: (Ty));
cast_impl!((InferenceVar) <: (Variable) <: (Ty));
cast_impl!((BoundVar) <: (Variable) <: (Ty));
cast_impl!(Lt);
cast_impl!(LtData::Variable(Variable));
cast_impl!((InferenceVar) <: (Variable) <: (LtData));
cast_impl!((PlaceholderVar) <: (Variable) <: (LtData));
cast_impl!((BoundVar) <: (Variable) <: (LtData));
cast_impl!((PlaceholderVar) <: (LtData) <: (Lt));
cast_impl!((InferenceVar) <: (LtData) <: (Lt));
cast_impl!((BoundVar) <: (LtData) <: (Lt));
cast_impl!(Variable::PlaceholderVar(PlaceholderVar));
cast_impl!(Variable::InferenceVar(InferenceVar));
cast_impl!(Variable::BoundVar(BoundVar));
