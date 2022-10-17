// Classification
//
// Predicate
//
// Goal -- something that can be proven
//
// Hypothesis -- something

use formality_core::all_into::AllInto;
use formality_core::interned::Interner;
use formality_core::interned::{Internable, Interned};
use formality_macros::Fold;

use crate::from_impl;

use super::Binder;
use super::Parameter;
use super::Parameters;
use super::TraitId;
use super::Ty;
use super::{AliasTy, KindedVarIndex};

pub type Fallible<T> = anyhow::Result<T>;

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AtomicPredicate {
    IsImplemented(TraitRef),
    HasImpl(TraitRef),
    NormalizesTo(AliasTy, Ty),
    WellFormed(Ty),
    // more to come
}

impl TraitRef {
    pub fn is_implemented(&self) -> AtomicPredicate {
        AtomicPredicate::IsImplemented(self.clone())
    }

    pub fn has_impl(&self) -> AtomicPredicate {
        AtomicPredicate::HasImpl(self.clone())
    }
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AtomicRelation {
    /// `T1 == T2` etc
    Equals(Parameter, Parameter),

    /// `T1 <: T2` or `L1 <: L2`
    Sub(Parameter, Parameter),

    /// `P : P`
    Outlives(Parameter, Parameter),
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitRef {
    pub trait_id: TraitId,
    pub parameters: Parameters,
}

impl TraitRef {
    pub fn new(id: TraitId, parameters: impl AllInto<Parameter>) -> Self {
        Self {
            trait_id: id,
            parameters: parameters.all_into(),
        }
    }
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Predicate {
    data: Interned<PredicateData>,
}

impl Predicate {
    pub fn data(&self) -> &PredicateData {
        &self.data
    }
}

impl<T> From<T> for Predicate
where
    T: Into<PredicateData>,
{
    fn from(v: T) -> Self {
        let v: PredicateData = v.into();
        Predicate {
            data: Interned::from(v),
        }
    }
}

impl Internable for PredicateData {
    fn table() -> &'static Interner<Self> {
        lazy_static::lazy_static! {
            static ref INTERNER: Interner<PredicateData> = Interner::default();
        }
        &*INTERNER
    }
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PredicateData {
    AtomicPredicate(AtomicPredicate),
    AtomicRelation(AtomicRelation),
    ForAll(Binder<Predicate>),
    Implies(Vec<Predicate>, Predicate),
}

from_impl!(impl From<AtomicPredicate> for PredicateData);
from_impl!(impl From<AtomicRelation> for PredicateData);

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Goal {
    data: Interned<GoalData>,
}

impl Goal {
    pub fn data(&self) -> &GoalData {
        &self.data
    }
}

impl<T> From<T> for Goal
where
    T: Into<GoalData>,
{
    fn from(v: T) -> Self {
        let v: GoalData = v.into();
        Self {
            data: Interned::from(v),
        }
    }
}

impl Internable for GoalData {
    fn table() -> &'static Interner<Self> {
        lazy_static::lazy_static! {
            static ref INTERNER: Interner<GoalData> = Interner::default();
        }
        &*INTERNER
    }
}

pub type Goals = Vec<Goal>;

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GoalData {
    AtomicPredicate(AtomicPredicate),
    AtomicRelation(AtomicRelation),
    ForAll(Binder<Goal>),
    Exists(Binder<Goal>),
    Implies(Vec<Hypothesis>, Goal),
    Any(Vec<Goal>),
    All(Vec<Goal>),
    CoherenceMode(Goal),
}

from_impl!(impl From<AtomicPredicate> for GoalData);
from_impl!(impl From<AtomicRelation> for GoalData);

impl Goal {
    pub fn for_all(names: &[KindedVarIndex], data: impl Into<Goal>) -> Self {
        GoalData::ForAll(Binder::new(names, data.into())).into()
    }

    pub fn exists(names: &[KindedVarIndex], data: impl Into<Goal>) -> Self {
        GoalData::Exists(Binder::new(names, data.into())).into()
    }

    pub fn implies(conditions: impl AllInto<Hypothesis>, consequence: impl Into<Goal>) -> Self {
        GoalData::Implies(conditions.all_into(), consequence.into()).into()
    }

    pub fn all(goals: impl AllInto<Goal>) -> Self {
        GoalData::All(goals.all_into()).into()
    }

    pub fn any(goals: impl AllInto<Goal>) -> Self {
        GoalData::Any(goals.all_into()).into()
    }

    pub fn coherence_mode(goal: impl Into<Goal>) -> Self {
        GoalData::CoherenceMode(goal.into()).into()
    }
}

pub type ProgramClause = Hypothesis;

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Hypothesis {
    data: Interned<HypothesisData>,
}

impl Hypothesis {
    pub fn data(&self) -> &HypothesisData {
        &self.data
    }
}

impl<T> From<T> for Hypothesis
where
    T: Into<HypothesisData>,
{
    fn from(v: T) -> Self {
        let v: HypothesisData = v.into();
        Hypothesis {
            data: Interned::from(v),
        }
    }
}

impl Internable for HypothesisData {
    fn table() -> &'static Interner<Self> {
        lazy_static::lazy_static! {
            static ref INTERNER: Interner<HypothesisData> = Interner::default();
        }
        &*INTERNER
    }
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HypothesisData {
    AtomicPredicate(AtomicPredicate),
    AtomicRelation(AtomicRelation),
    ForAll(Binder<Hypothesis>),
    Implies(Vec<Goal>, Hypothesis),
    CoherenceMode,
}

from_impl!(impl From<AtomicPredicate> for HypothesisData);
from_impl!(impl From<AtomicRelation> for HypothesisData);

impl Hypothesis {
    pub fn for_all(names: &[KindedVarIndex], data: impl Into<Hypothesis>) -> Self {
        HypothesisData::ForAll(Binder::new(names, data)).into()
    }

    pub fn implies(conditions: impl AllInto<Goal>, consequence: impl Into<Hypothesis>) -> Self {
        HypothesisData::Implies(conditions.all_into(), consequence.into()).into()
    }

    pub fn coherence_mode() -> Self {
        HypothesisData::CoherenceMode.into()
    }
}

pub type Hypotheses = Vec<Hypothesis>;

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Invariant {
    for_all: Binder<InvariantImplication>,
}

#[derive(Fold, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvariantImplication {
    conditions: Vec<AtomicPredicate>,
    consequence: AtomicPredicate,
}

pub type Invariants = Vec<Invariant>;

impl From<Predicate> for Goal {
    fn from(value: Predicate) -> Self {
        match value.data() {
            PredicateData::AtomicPredicate(a) => a.clone().into(),
            PredicateData::AtomicRelation(a) => a.clone().into(),
            PredicateData::ForAll(binder) => GoalData::ForAll(binder.clone().into()).into(),
            PredicateData::Implies(p, q) => {
                GoalData::Implies(p.clone().all_into(), q.clone().into()).into()
            }
        }
    }
}

impl From<Predicate> for Hypothesis {
    fn from(value: Predicate) -> Self {
        match value.data() {
            PredicateData::AtomicPredicate(a) => a.clone().into(),
            PredicateData::AtomicRelation(a) => a.clone().into(),
            PredicateData::ForAll(binder) => HypothesisData::ForAll(binder.clone().into()).into(),
            PredicateData::Implies(p, q) => {
                HypothesisData::Implies(p.clone().all_into(), q.clone().into()).into()
            }
        }
    }
}
