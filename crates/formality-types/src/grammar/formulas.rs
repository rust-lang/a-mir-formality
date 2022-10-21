// Classification
//
// Predicate
//
// Goal -- something that can be proven
//
// Hypothesis -- something

use std::collections::BTreeSet;
use std::sync::Arc;

use formality_macros::term;

use crate::cast::Downcast;
use crate::cast::To;
use crate::cast::Upcast;
use crate::cast::UpcastFrom;
use crate::from_term_impl;

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
    pub fn new(id: &TraitId, parameters: impl Upcast<Vec<Parameter>>) -> Self {
        Self {
            trait_id: id.clone(),
            parameters: parameters.upcast(),
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

impl UpcastFrom<PredicateData> for Predicate {
    fn upcast_from(v: PredicateData) -> Self {
        Predicate { data: Arc::new(v) }
    }
}

impl Downcast<Predicate> for PredicateData {
    fn downcast(t: &Predicate) -> Option<Self> {
        Some(t.data().clone())
    }
}

from_term_impl!(impl UpcastFrom<AtomicPredicate> for Predicate via PredicateData);
from_term_impl!(impl UpcastFrom<AtomicRelation> for Predicate via PredicateData);

#[term]
pub enum PredicateData {
    AtomicPredicate(AtomicPredicate),
    AtomicRelation(AtomicRelation),
    ForAll(Binder<Predicate>),
    #[grammar($v0 => $v1)]
    Implies(Vec<Predicate>, Predicate),
}

from_term_impl!(impl UpcastFrom<AtomicPredicate> for PredicateData);
from_term_impl!(impl UpcastFrom<AtomicRelation> for PredicateData);

#[term]
pub struct Goal {
    data: Arc<GoalData>,
}

impl Goal {
    pub fn data(&self) -> &GoalData {
        &self.data
    }
}

impl UpcastFrom<GoalData> for Goal {
    fn upcast_from(v: GoalData) -> Self {
        Self { data: Arc::new(v) }
    }
}

from_term_impl!(impl UpcastFrom<AtomicPredicate> for Goal via GoalData);
from_term_impl!(impl UpcastFrom<AtomicRelation> for Goal via GoalData);

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
    Ambiguous,
}

from_term_impl!(impl UpcastFrom<AtomicPredicate> for GoalData);
from_term_impl!(impl UpcastFrom<AtomicRelation> for GoalData);

impl Goal {
    pub fn ambiguous() -> Self {
        GoalData::Ambiguous.upcast()
    }

    pub fn for_all(names: &[KindedVarIndex], data: impl Upcast<Goal>) -> Self {
        GoalData::ForAll(Binder::new(names, data.upcast())).upcast()
    }

    pub fn exists(names: &[KindedVarIndex], data: impl Upcast<Goal>) -> Self {
        GoalData::Exists(Binder::new(names, data.upcast())).upcast()
    }

    pub fn implies(
        conditions: impl Upcast<Vec<Hypothesis>>,
        consequence: impl Upcast<Goal>,
    ) -> Self {
        GoalData::Implies(conditions.upcast(), consequence.upcast()).upcast()
    }

    pub fn all(goals: impl Upcast<Vec<Goal>>) -> Self {
        GoalData::All(goals.upcast()).upcast()
    }

    pub fn any(goals: impl Upcast<Vec<Goal>>) -> Self {
        GoalData::Any(goals.upcast()).upcast()
    }

    pub fn coherence_mode(goal: impl Upcast<Goal>) -> Self {
        GoalData::CoherenceMode(goal.upcast()).upcast()
    }

    /// Goal that `p1 == p2`
    pub fn eq(p1: impl Upcast<Parameter>, p2: impl Upcast<Parameter>) -> Self {
        AtomicRelation::Equals(p1.upcast(), p2.upcast()).upcast()
    }

    /// Goal that `p1 <: p2`
    pub fn sub(p1: impl Upcast<Parameter>, p2: impl Upcast<Parameter>) -> Self {
        AtomicRelation::Sub(p1.upcast(), p2.upcast()).upcast()
    }

    /// Goal that `p1: p2`
    pub fn outlives(p1: impl Upcast<Parameter>, p2: impl Upcast<Parameter>) -> Self {
        AtomicRelation::Outlives(p1.upcast(), p2.upcast()).upcast()
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

    /// True if this hypotheses could possible match predicate, for some assignment
    /// of its bound variables. This is used by solver to prune down the paths
    /// we search, with the goal of improving readability (i.e., if this function
    /// just returned `false`, the solver should still work).
    pub fn could_match(&self, predicate: &AtomicPredicate) -> bool {
        match self.data() {
            HypothesisData::AtomicPredicate(p) => p.debone().0 == predicate.debone().0,
            HypothesisData::AtomicRelation(_) => false,
            HypothesisData::ForAll(binder) => binder.peek().could_match(predicate),
            HypothesisData::Implies(_, consequence) => consequence.could_match(predicate),
            HypothesisData::CoherenceMode => false,
        }
    }
}

impl UpcastFrom<HypothesisData> for Hypothesis {
    fn upcast_from(v: HypothesisData) -> Self {
        Hypothesis { data: Arc::new(v) }
    }
}

from_term_impl!(impl UpcastFrom<AtomicPredicate> for Hypothesis via HypothesisData);
from_term_impl!(impl UpcastFrom<AtomicRelation> for Hypothesis via HypothesisData);

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

from_term_impl!(impl UpcastFrom<AtomicPredicate> for HypothesisData);
from_term_impl!(impl UpcastFrom<AtomicRelation> for HypothesisData);

impl Hypothesis {
    pub fn for_all(names: &[KindedVarIndex], data: impl Upcast<Hypothesis>) -> Self {
        HypothesisData::ForAll(Binder::new(names, data.upcast())).upcast()
    }

    pub fn implies(
        conditions: impl Upcast<Vec<Goal>>,
        consequence: impl Upcast<Hypothesis>,
    ) -> Self {
        HypothesisData::Implies(conditions.upcast(), consequence.upcast()).upcast()
    }

    pub fn coherence_mode() -> Self {
        HypothesisData::CoherenceMode.upcast()
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
                consequence: _,
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
        kinded_var_indices.iter().map(|&kvi| kvi.to()).all(|p: Parameter| parameters.contains(&p)) &&

        // finally, each of the items in the parameter list must be distinct from the others
        parameters.iter().collect::<BTreeSet<_>>().len() == parameters.len()
        };

        assert!(condition, "invalid invariant: {self:?}");
    }
}

impl UpcastFrom<Predicate> for Goal {
    fn upcast_from(value: Predicate) -> Self {
        match value.data() {
            PredicateData::AtomicPredicate(a) => a.to(),
            PredicateData::AtomicRelation(a) => a.to(),
            PredicateData::ForAll(binder) => GoalData::ForAll(binder.to()).upcast(),
            PredicateData::Implies(p, q) => GoalData::Implies(p.to(), q.to()).upcast(),
        }
    }
}

impl UpcastFrom<Predicate> for Hypothesis {
    fn upcast_from(value: Predicate) -> Self {
        match value.data() {
            PredicateData::AtomicPredicate(a) => a.to(),
            PredicateData::AtomicRelation(a) => a.to(),
            PredicateData::ForAll(binder) => HypothesisData::ForAll(binder.to()).upcast(),
            PredicateData::Implies(p, q) => HypothesisData::Implies(p.to(), q.to()).upcast(),
        }
    }
}
