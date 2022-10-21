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

use crate::from_into_term::FromTerm;
use crate::from_into_term::IntoTerm;
use crate::from_into_term::To;
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
    pub fn new(id: &TraitId, parameters: impl IntoTerm<Vec<Parameter>>) -> Self {
        Self {
            trait_id: id.clone(),
            parameters: parameters.into_term(),
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

impl FromTerm<PredicateData> for Predicate {
    fn from_term(v: PredicateData) -> Self {
        Predicate { data: Arc::new(v) }
    }
}

from_term_impl!(impl FromTerm<AtomicPredicate> for Predicate via PredicateData);
from_term_impl!(impl FromTerm<AtomicRelation> for Predicate via PredicateData);

#[term]
pub enum PredicateData {
    AtomicPredicate(AtomicPredicate),
    AtomicRelation(AtomicRelation),
    ForAll(Binder<Predicate>),
    #[grammar($v0 => $v1)]
    Implies(Vec<Predicate>, Predicate),
}

from_term_impl!(impl FromTerm<AtomicPredicate> for PredicateData);
from_term_impl!(impl FromTerm<AtomicRelation> for PredicateData);

#[term]
pub struct Goal {
    data: Arc<GoalData>,
}

impl Goal {
    pub fn data(&self) -> &GoalData {
        &self.data
    }
}

impl FromTerm<GoalData> for Goal {
    fn from_term(v: GoalData) -> Self {
        Self { data: Arc::new(v) }
    }
}

from_term_impl!(impl FromTerm<AtomicPredicate> for Goal via GoalData);
from_term_impl!(impl FromTerm<AtomicRelation> for Goal via GoalData);

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

from_term_impl!(impl FromTerm<AtomicPredicate> for GoalData);
from_term_impl!(impl FromTerm<AtomicRelation> for GoalData);

impl Goal {
    pub fn ambiguous() -> Self {
        GoalData::Ambiguous.into_term()
    }

    pub fn for_all(names: &[KindedVarIndex], data: impl IntoTerm<Goal>) -> Self {
        GoalData::ForAll(Binder::new(names, data.into_term())).into_term()
    }

    pub fn exists(names: &[KindedVarIndex], data: impl IntoTerm<Goal>) -> Self {
        GoalData::Exists(Binder::new(names, data.into_term())).into_term()
    }

    pub fn implies(
        conditions: impl IntoTerm<Vec<Hypothesis>>,
        consequence: impl IntoTerm<Goal>,
    ) -> Self {
        GoalData::Implies(conditions.into_term(), consequence.into_term()).into_term()
    }

    pub fn all(goals: impl IntoTerm<Vec<Goal>>) -> Self {
        GoalData::All(goals.into_term()).into_term()
    }

    pub fn any(goals: impl IntoTerm<Vec<Goal>>) -> Self {
        GoalData::Any(goals.into_term()).into_term()
    }

    pub fn coherence_mode(goal: impl IntoTerm<Goal>) -> Self {
        GoalData::CoherenceMode(goal.into_term()).into_term()
    }

    /// Goal that `p1 == p2`
    pub fn eq(p1: impl IntoTerm<Parameter>, p2: impl IntoTerm<Parameter>) -> Self {
        AtomicRelation::Equals(p1.into_term(), p2.into_term()).into_term()
    }

    /// Goal that `p1 <: p2`
    pub fn sub(p1: impl IntoTerm<Parameter>, p2: impl IntoTerm<Parameter>) -> Self {
        AtomicRelation::Sub(p1.into_term(), p2.into_term()).into_term()
    }

    /// Goal that `p1: p2`
    pub fn outlives(p1: impl IntoTerm<Parameter>, p2: impl IntoTerm<Parameter>) -> Self {
        AtomicRelation::Outlives(p1.into_term(), p2.into_term()).into_term()
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

impl FromTerm<HypothesisData> for Hypothesis {
    fn from_term(v: HypothesisData) -> Self {
        Hypothesis { data: Arc::new(v) }
    }
}

from_term_impl!(impl FromTerm<AtomicPredicate> for Hypothesis via HypothesisData);
from_term_impl!(impl FromTerm<AtomicRelation> for Hypothesis via HypothesisData);

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

from_term_impl!(impl FromTerm<AtomicPredicate> for HypothesisData);
from_term_impl!(impl FromTerm<AtomicRelation> for HypothesisData);

impl Hypothesis {
    pub fn for_all(names: &[KindedVarIndex], data: impl IntoTerm<Hypothesis>) -> Self {
        HypothesisData::ForAll(Binder::new(names, data.into_term())).into_term()
    }

    pub fn implies(
        conditions: impl IntoTerm<Vec<Goal>>,
        consequence: impl IntoTerm<Hypothesis>,
    ) -> Self {
        HypothesisData::Implies(conditions.into_term(), consequence.into_term()).into_term()
    }

    pub fn coherence_mode() -> Self {
        HypothesisData::CoherenceMode.into_term()
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

impl FromTerm<Predicate> for Goal {
    fn from_term(value: Predicate) -> Self {
        match value.data() {
            PredicateData::AtomicPredicate(a) => a.to_term(),
            PredicateData::AtomicRelation(a) => a.to_term(),
            PredicateData::ForAll(binder) => GoalData::ForAll(binder.to_term()).into_term(),
            PredicateData::Implies(p, q) => GoalData::Implies(p.to_term(), q.to_term()).into_term(),
        }
    }
}

impl FromTerm<Predicate> for Hypothesis {
    fn from_term(value: Predicate) -> Self {
        match value.data() {
            PredicateData::AtomicPredicate(a) => a.to_term(),
            PredicateData::AtomicRelation(a) => a.to_term(),
            PredicateData::ForAll(binder) => HypothesisData::ForAll(binder.to_term()).into_term(),
            PredicateData::Implies(p, q) => {
                HypothesisData::Implies(p.to_term(), q.to_term()).into_term()
            }
        }
    }
}
