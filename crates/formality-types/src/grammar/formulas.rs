// Classification
//
// Predicate
//
// Goal -- something that can be proven
//
// Hypothesis -- something

use std::collections::BTreeSet;
use std::sync::Arc;

use formality_core::all_into::AllInto;
use formality_macros::term;

use crate::from_impl;

use super::AdtId;
use super::AliasName;
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
    WellFormedTy(Ty),
    #[grammar(well_formed_trait_ref($v0))]
    WellFormedTraitRef(TraitRef),
}

/// The "skeleton" of an atomic predicate is the kernel that contains
/// nothing unifiable and identifies the kind of predicate.
/// If the skeleton's don't match, they are distinct predicates.
#[term]
pub enum AtomicPredicateSkeleton {
    #[grammar(is_implemented($v0))]
    IsImplemented(TraitId),
    #[grammar(has_impl($v0))]
    HasImpl(TraitId),
    #[grammar(normalizes_to($v0))]
    NormalizesTo(AliasName),
    #[grammar(well_formed_ty)]
    WellFormedTy,
    #[grammar(well_formed_trait_ref($v0))]
    WellFormedTraitRef(TraitId),
}

impl AtomicPredicate {
    /// Separate an atomic predicate into the "skeleton" (which can be compared for equality using `==`)
    /// and the parameters (which must be related).
    pub fn debone(&self) -> (AtomicPredicateSkeleton, Vec<Parameter>) {
        match self {
            AtomicPredicate::IsImplemented(TraitRef {
                trait_id,
                parameters,
            }) => (
                AtomicPredicateSkeleton::IsImplemented(trait_id.clone()),
                parameters.clone(),
            ),
            AtomicPredicate::HasImpl(TraitRef {
                trait_id,
                parameters,
            }) => (
                AtomicPredicateSkeleton::HasImpl(trait_id.clone()),
                parameters.clone(),
            ),
            AtomicPredicate::NormalizesTo(AliasTy { name, parameters }, ty) => (
                AtomicPredicateSkeleton::NormalizesTo(name.clone()),
                parameters
                    .iter()
                    .cloned()
                    .chain(Some(ty.to_parameter()))
                    .collect(),
            ),
            AtomicPredicate::WellFormedTy(ty) => (
                AtomicPredicateSkeleton::WellFormedTy,
                vec![ty.to_parameter()],
            ),
            AtomicPredicate::WellFormedTraitRef(TraitRef {
                trait_id,
                parameters,
            }) => (
                AtomicPredicateSkeleton::WellFormedTraitRef(trait_id.clone()),
                parameters.clone(),
            ),
        }
    }
}

impl TraitRef {
    pub fn is_implemented(&self) -> AtomicPredicate {
        AtomicPredicate::IsImplemented(self.clone())
    }

    pub fn has_impl(&self) -> AtomicPredicate {
        AtomicPredicate::HasImpl(self.clone())
    }

    pub fn well_formed(&self) -> AtomicPredicate {
        AtomicPredicate::WellFormedTraitRef(self.clone())
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
    #[grammar(coherence_mode)]
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

#[term]
pub struct Invariant {
    pub binder: Binder<InvariantImplication>,
}

#[term($condition => $consequence)]
pub struct InvariantImplication {
    /// Invariant: each parameter on the condition will be a distinct variable
    /// that appears in the invariant binder.
    pub condition: AtomicPredicate,
    pub consequence: AtomicPredicate,
}

impl Invariant {
    pub fn assert(&self) {
        let (
            kinded_var_indices,
            InvariantImplication {
                condition,
                consequence,
            },
        ) = self.binder.open();

        // the set {0..n} of bound variables from the invariant (tracked by their index within binder)
        let indices: BTreeSet<usize> = (0..kinded_var_indices.len()).collect();

        // the parameters to the condition: each parameter must be a distinct variable from that set
        let (_, parameters) = condition.debone();

        let condition = {
            // first, number of parameters should be equal to number of bound variables
            parameters.len() == indices.len() &&

        // second, each of the bound variables should appear in the parameter list
        kinded_var_indices.iter().map(|&kvi| Parameter::from(kvi)).all(|p| parameters.contains(&p)) &&

        // finally, each of the items in the parameter list must be distinct from the others
        parameters.iter().collect::<BTreeSet<_>>().len() == parameters.len()
        };

        assert!(condition, "invalid invariant: {self:?}");
    }
}

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
