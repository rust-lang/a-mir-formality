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

use super::{AdtId, AssociatedItemId, Binder, FnId, TraitId};

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

cast_impl!(InferenceVar);

impl Visit for InferenceVar {
    fn free_variables(&self) -> Vec<Variable> {
        vec![self.upcast()]
    }

    fn size(&self) -> usize {
        1
    }

    fn assert_valid(&self) {}
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

/// A *universal variable* is a dummy variable about which nothing is known except
/// that which we see in the environment. When we want to prove something
/// is true for all `T` (`∀T`), we replace `T` with a universal variable.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UniversalVar {
    pub kind: ParameterKind,
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

    pub fn is_variable(&self) -> bool {
        self.as_variable().is_some()
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

impl Visit for LtData {
    fn free_variables(&self) -> Vec<Variable> {
        match self {
            LtData::Variable(v) => {
                if v.is_free() {
                    vec![v.clone()]
                } else {
                    vec![]
                }
            }
            LtData::Static => vec![],
        }
    }

    fn size(&self) -> usize {
        match self {
            LtData::Variable(v) => v.size(),
            LtData::Static => 1,
        }
    }

    fn assert_valid(&self) {
        match self {
            LtData::Variable(v) => v.assert_valid(),
            LtData::Static => (),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Variable {
    UniversalVar(UniversalVar),
    InferenceVar(InferenceVar),
    BoundVar(BoundVar),
}

cast_impl!(Variable);

impl Variable {
    pub fn kind(&self) -> ParameterKind {
        match self {
            Variable::UniversalVar(v) => v.kind,
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
    /// if it is an inference variable, a universal, or has a debruijn
    /// index of `None`. The latter occurs when you `open` a binder (and before
    /// you close it back up again).
    pub fn is_free(&self) -> bool {
        match self {
            Variable::UniversalVar(_)
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

    pub fn is_universal(&self) -> bool {
        match self {
            Variable::UniversalVar(_) => true,
            Variable::InferenceVar(_) => false,
            Variable::BoundVar(_) => false,
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

    fn size(&self) -> usize {
        1
    }

    fn assert_valid(&self) {}
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
cast_impl!((UniversalVar) <: (Variable) <: (Parameter));

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

impl std::ops::Add<usize> for VarIndex {
    type Output = VarIndex;

    fn add(self, rhs: usize) -> Self::Output {
        VarIndex {
            index: self.index + rhs,
        }
    }
}

#[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Substitution {
    map: Map<Variable, Parameter>,
}

impl Substitution {
    /// Returns the variables that will be substituted for a new value.
    pub fn domain(&self) -> BTreeSet<Variable> {
        self.map.keys().cloned().collect()
    }

    /// Returns the parameters that that will be substituted for a new value.
    pub fn range(&self) -> BTreeSet<Parameter> {
        self.map.values().cloned().collect()
    }

    /// True if `v` is in this substitution's domain
    pub fn maps(&self, v: Variable) -> bool {
        self.map.contains_key(&v)
    }

    pub fn iter(&self) -> impl Iterator<Item = (Variable, Parameter)> + '_ {
        self.map.iter().map(|(v, p)| (*v, p.clone()))
    }

    /// An empty substitution is just the identity function.
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

impl std::fmt::Debug for Substitution {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = f.debug_set();
        for (v, p) in self.iter() {
            f.entry(&Entry { v, p });
            struct Entry {
                v: Variable,
                p: Parameter,
            }
            impl std::fmt::Debug for Entry {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{:?} => {:?}", self.v, self.p)
                }
            }
        }
        f.finish()
    }
}

impl<Vs> std::ops::SubAssign<Vs> for Substitution
where
    Vs: Upcast<Vec<Variable>>,
{
    fn sub_assign(&mut self, rhs: Vs) {
        let rhs: Vec<Variable> = rhs.upcast();

        for v in rhs {
            self.map.remove(&v);
        }
    }
}

impl IntoIterator for Substitution {
    type Item = (Variable, Parameter);

    type IntoIter = std::collections::btree_map::IntoIter<Variable, Parameter>;

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}

impl<A, B> Extend<(A, B)> for Substitution
where
    A: Upcast<Variable>,
    B: Upcast<Parameter>,
{
    fn extend<T: IntoIterator<Item = (A, B)>>(&mut self, iter: T) {
        self.map
            .extend(iter.into_iter().map(|(v, p)| (v.upcast(), p.upcast())));
    }
}

impl<A, B> FromIterator<(A, B)> for Substitution
where
    A: Upcast<Variable>,
    B: Upcast<Parameter>,
{
    fn from_iter<T: IntoIterator<Item = (A, B)>>(iter: T) -> Self {
        let mut s = Substitution::default();
        s.extend(iter);
        s
    }
}

impl<A, B> UpcastFrom<(A, B)> for Substitution
where
    A: Upcast<Variable>,
    B: Upcast<Parameter>,
{
    fn upcast_from(term: (A, B)) -> Self {
        let term: (Variable, Parameter) = term.upcast();
        std::iter::once(term).collect()
    }
}

impl Substitution {
    pub fn apply<T: Fold>(&self, t: &T) -> T {
        t.substitute(&mut |v| self.map.get(&v).cloned())
    }

    pub fn get(&self, v: Variable) -> Option<Parameter> {
        self.map.get(&v).cloned()
    }
}

impl Fold for Substitution {
    fn substitute(&self, substitution_fn: crate::fold::SubstitutionFn<'_>) -> Self {
        self.iter()
            .map(|(v, p)| (v, p.substitute(substitution_fn)))
            .collect()
    }
}

impl Visit for Substitution {
    fn free_variables(&self) -> Vec<Variable> {
        let mut v = self.range().free_variables();
        v.extend(self.domain());
        v
    }

    fn size(&self) -> usize {
        self.range().iter().map(|r| r.size()).sum()
    }

    fn assert_valid(&self) {
        self.range().assert_valid()
    }
}

impl std::ops::Index<Variable> for Substitution {
    type Output = Parameter;

    fn index(&self, index: Variable) -> &Self::Output {
        &self.map[&index]
    }
}

/// A substitution that is only between variables.
/// These are reversible.
#[derive(Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarSubstitution {
    map: Map<Variable, Variable>,
}

impl<A, B> Extend<(A, B)> for VarSubstitution
where
    A: Upcast<Variable>,
    B: Upcast<Variable>,
{
    fn extend<T: IntoIterator<Item = (A, B)>>(&mut self, iter: T) {
        self.map
            .extend(iter.into_iter().map(|(v, p)| (v.upcast(), p.upcast())));
    }
}

impl<A, B> FromIterator<(A, B)> for VarSubstitution
where
    A: Upcast<Variable>,
    B: Upcast<Variable>,
{
    fn from_iter<T: IntoIterator<Item = (A, B)>>(iter: T) -> Self {
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

    pub fn map_var(&self, v: Variable) -> Option<Variable> {
        self.map.get(&v).copied()
    }

    pub fn maps_var(&self, v: Variable) -> bool {
        self.map.contains_key(&v)
    }

    pub fn insert_mapping(&mut self, from: impl Upcast<Variable>, to: impl Upcast<Variable>) {
        let x = self.map.insert(from.upcast(), to.upcast());
        assert!(x.is_none());
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
cast_impl!((UniversalVar) <: (Variable) <: (TyData));
cast_impl!((InferenceVar) <: (Variable) <: (TyData));
cast_impl!((BoundVar) <: (Variable) <: (TyData));
cast_impl!((ScalarId) <: (RigidTy) <: (TyData));
cast_impl!((UniversalVar) <: (Variable) <: (Ty));
cast_impl!((InferenceVar) <: (Variable) <: (Ty));
cast_impl!((BoundVar) <: (Variable) <: (Ty));
cast_impl!(Lt);
cast_impl!(LtData::Variable(Variable));
cast_impl!((InferenceVar) <: (Variable) <: (LtData));
cast_impl!((UniversalVar) <: (Variable) <: (LtData));
cast_impl!((BoundVar) <: (Variable) <: (LtData));
cast_impl!((UniversalVar) <: (LtData) <: (Lt));
cast_impl!((InferenceVar) <: (LtData) <: (Lt));
cast_impl!((BoundVar) <: (LtData) <: (Lt));
cast_impl!(Variable::UniversalVar(UniversalVar));
cast_impl!(Variable::InferenceVar(InferenceVar));
cast_impl!(Variable::BoundVar(BoundVar));
