// Classification
//
// Predicate
//
// Goal -- something that can be proven
//
// Hypothesis -- something

use std::sync::Arc;

use formality_core::all_into::AllInto;
use formality_macros::term;

use crate::from_impl;

use super::Binder;
use super::Parameter;
use super::Parameters;
use super::TraitId;
use super::Ty;
use super::{AliasTy, KindedVarIndex};

pub type Fallible<T> = anyhow::Result<T>;

#[term]
pub enum AtomicPredicate {
    #[grammar(is_implemented($v0))]
    IsImplemented(TraitRef),
    #[grammar(has_impl($v0))]
    HasImpl(TraitRef),
    #[grammar(normalizes_to($v0 -> $v1))]
    NormalizesTo(AliasTy, Ty),
    #[grammar(well_formed($v0))]
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

#[term]
pub enum AtomicRelation {
    /// `T1 == T2` etc
    #[grammar(($v0 == $v1))]
    Equals(Parameter, Parameter),

    /// `T1 <: T2` or `L1 <: L2`
    #[grammar(($v0 <: $v1))]
    Sub(Parameter, Parameter),

    /// `P : P`
    #[grammar(($v0 : $v1))]
    Outlives(Parameter, Parameter),
}

#[term($trait_id < $,parameters >)]
pub struct TraitRef {
    pub trait_id: TraitId,
    pub parameters: Parameters,
}

impl TraitRef {
    pub fn new(id: &TraitId, parameters: impl AllInto<Parameter>) -> Self {
        Self {
            trait_id: id.clone(),
            parameters: parameters.all_into(),
        }
    }
}

#[term]
pub struct Predicate {
    data: Arc<PredicateData>,
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
        Predicate { data: Arc::new(v) }
    }
}

#[term]
pub enum PredicateData {
    AtomicPredicate(AtomicPredicate),
    AtomicRelation(AtomicRelation),
    ForAll(Binder<Predicate>),
    #[grammar($v0 => $v1)]
    Implies(Vec<Predicate>, Predicate),
}

from_impl!(impl From<AtomicPredicate> for PredicateData);
from_impl!(impl From<AtomicRelation> for PredicateData);

#[term]
pub struct Goal {
    data: Arc<GoalData>,
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
        Self { data: Arc::new(v) }
    }
}

pub type Goals = Vec<Goal>;

#[term]
pub enum GoalData {
    AtomicPredicate(AtomicPredicate),
    AtomicRelation(AtomicRelation),
    #[grammar(forall $v0)]
    ForAll(Binder<Goal>),
    #[grammar(exists $v0)]
    Exists(Binder<Goal>),
    #[grammar($v0 => $v1)]
    Implies(Vec<Hypothesis>, Goal),
    #[grammar(any($,v0))]
    Any(Vec<Goal>),
    #[grammar(all($,v0))]
    All(Vec<Goal>),
    #[grammar(coherence_mode($v0))]
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

#[term]
pub struct Hypothesis {
    data: Arc<HypothesisData>,
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
        Hypothesis { data: Arc::new(v) }
    }
}

#[term]
pub enum HypothesisData {
    AtomicPredicate(AtomicPredicate),
    AtomicRelation(AtomicRelation),
    #[grammar(forall $v0)]
    ForAll(Binder<Hypothesis>),
    #[grammar($v0 => $v1)]
    Implies(Vec<Goal>, Hypothesis),
    CoherenceMode,
}

from_impl!(impl From<AtomicPredicate> for HypothesisData);
from_impl!(impl From<AtomicRelation> for HypothesisData);

impl Hypothesis {
    pub fn for_all(names: &[KindedVarIndex], data: impl Into<Hypothesis>) -> Self {
        HypothesisData::ForAll(Binder::new(names, data.into())).into()
    }

    pub fn implies(conditions: impl AllInto<Goal>, consequence: impl Into<Hypothesis>) -> Self {
        HypothesisData::Implies(conditions.all_into(), consequence.into()).into()
    }

    pub fn coherence_mode() -> Self {
        HypothesisData::CoherenceMode.into()
    }
}

pub type Hypotheses = Vec<Hypothesis>;

#[term(forall $for_all)]
pub struct Invariant {
    for_all: Binder<InvariantImplication>,
}

#[term($conditions => $consequence)]
pub struct InvariantImplication {
    pub conditions: Vec<AtomicPredicate>,
    pub consequence: AtomicPredicate,
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
