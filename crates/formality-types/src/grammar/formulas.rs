// Classification
//
// Predicate
//
// Goal -- something that can be proven
//
// Hypothesis -- something

use formality_core::interned::Interned;

use super::AliasTy;
use super::Binder;
use super::Parameter;
use super::Parameters;
use super::TraitId;
use super::Ty;

pub type Fallible<T> = anyhow::Result<T>;

// QUESTION:
// * Should we make this an "open-ended" set?
// * Some kind of interned atomic predicate kind that is only considered
//   equal if they are the same pointer.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AtomicPredicate {
    data: Interned<AtomicPredicateData>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AtomicPredicateData {
    IsImplemented(TraitRef),
    NormalizesTo(AliasTy, Ty),
    WellFormed(Ty),
    // more to come
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AtomicRelation {
    Equals(Parameter, Parameter),

    // `T1 <: T2` or `L1 <: L2`
    Sub(Parameter, Parameter),

    // `P : P`
    Outlives(Parameter, Parameter),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitRef {
    pub trait_id: TraitId,
    pub parameters: Parameters,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Predicate {
    data: Interned<PredicateData>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PredicateData {
    AtomicPredicate(AtomicPredicate),
    AtomicRelation(AtomicRelation),
    ForAll(Binder<Predicate>),
    Implies(Vec<Predicate>, Predicate),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Goal {
    data: Interned<GoalData>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Hypothesis {
    data: Interned<HypothesisData>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HypothesisData {
    AtomicPredicate(AtomicPredicate),
    AtomicRelation(AtomicRelation),
    ForAll(Binder<Hypothesis>),
    Implies(Vec<Goal>, Hypothesis),
    CoherenceMode,
}
