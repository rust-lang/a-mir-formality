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
use crate::cast::DowncastFrom;
use crate::cast::To;
use crate::cast::Upcast;
use crate::cast::UpcastFrom;
use crate::cast_impl;
use crate::collections::Set;

use super::AliasName;
use super::Binder;
use super::Kinded;
use super::Parameter;
use super::Parameters;
use super::TraitId;
use super::Ty;
use super::{AliasTy, KindedVarIndex};

pub type Fallible<T> = anyhow::Result<T>;

#[term]
pub enum AtomicPredicate {
    IsImplemented(TraitRef),
    HasImpl(TraitRef),
    NormalizesTo(AliasTy, Ty),
    WellFormedTy(Ty),
    WellFormedTraitRef(TraitRef),
}

/// A coinductive predicate is one that can be proven via a cycle.
pub enum Coinductive {
    No,
    Yes,
}

impl std::ops::BitAnd for Coinductive {
    type Output = Coinductive;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Coinductive::Yes, Coinductive::Yes) => Coinductive::Yes,
            _ => Coinductive::No,
        }
    }
}

impl AtomicPredicate {
    /// True if this goal can be proven via a cycle. For example,
    /// it is ok for a `T: Debug` impl to require `T: Debug`.
    pub fn is_coinductive(&self) -> Coinductive {
        match self {
            AtomicPredicate::IsImplemented(_) => Coinductive::Yes,
            AtomicPredicate::HasImpl(_) => Coinductive::Yes,
            AtomicPredicate::NormalizesTo(_, _) => Coinductive::No,
            AtomicPredicate::WellFormedTy(_) => Coinductive::Yes,
            AtomicPredicate::WellFormedTraitRef(_) => Coinductive::Yes,
        }
    }
}

impl AliasTy {
    pub fn normalizes_to(&self, t: impl Upcast<Ty>) -> AtomicPredicate {
        AtomicPredicate::NormalizesTo(self.clone(), t.upcast())
    }
}

impl Ty {
    pub fn well_formed(&self) -> AtomicPredicate {
        AtomicPredicate::WellFormedTy(self.clone())
    }
}

impl Parameter {
    /// Well-formed goal for a parameter
    pub fn well_formed(&self) -> Goal {
        match self {
            Parameter::Ty(v) => v.well_formed().upcast(),

            // we always assume lifetimes are well-formed
            Parameter::Lt(_) => Goal::t(),
        }
    }
}

/// The "skeleton" of an atomic predicate is the kernel that contains
/// nothing unifiable and identifies the kind of predicate.
/// If the skeleton's don't match, they are distinct predicates.
#[term]
pub enum AtomicPredicateSkeleton {
    IsImplemented(TraitId),
    HasImpl(TraitId),
    NormalizesTo(AliasName),
    WellFormedTy,
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
    Equals(Parameter, Parameter),

    /// `T1 <: T2` or `L1 <: L2`
    Sub(Parameter, Parameter),

    /// `P : P`
    Outlives(Parameter, Parameter),
}

impl AtomicRelation {
    pub fn eq(p1: impl Upcast<Parameter>, p2: impl Upcast<Parameter>) -> Self {
        Self::Equals(p1.upcast(), p2.upcast())
    }
}

#[term($trait_id ( $,parameters ))]
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

#[term($data)]
pub struct Predicate {
    data: Arc<PredicateData>,
}

impl Predicate {
    pub fn for_all(names: &[KindedVarIndex], data: impl Upcast<Predicate>) -> Self {
        PredicateData::ForAll(Binder::new(names, data.upcast())).upcast()
    }

    pub fn data(&self) -> &PredicateData {
        &self.data
    }
}

impl UpcastFrom<PredicateData> for Predicate {
    fn upcast_from(v: PredicateData) -> Self {
        Predicate { data: Arc::new(v) }
    }
}

impl DowncastFrom<Predicate> for PredicateData {
    fn downcast_from(t: &Predicate) -> Option<Self> {
        Some(t.data().clone())
    }
}

/// "APR" == AtomicPredicateOrRelation
///
/// We need a better name for this lol.
#[term]
pub enum APR {
    #[cast]
    AtomicPredicate(AtomicPredicate),
    #[cast]
    AtomicRelation(AtomicRelation),
}

#[term]
pub enum PredicateData {
    #[cast]
    Atomic(APR),
    ForAll(Binder<Predicate>),
    Implies(Vec<Predicate>, Predicate),
}

#[term($data)]
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

impl Downcast<GoalData> for Goal {
    fn downcast(&self) -> Option<GoalData> {
        Some(self.data().clone())
    }
}

pub type Goals = Vec<Goal>;

#[term]
pub enum GoalData {
    #[cast]
    Atomic(APR),
    ForAll(Binder<Goal>),
    Exists(Binder<Goal>),
    Implies(Vec<Hypothesis>, Goal),
    #[grammar(any($,v0))]
    Any(Vec<Goal>),
    #[grammar(all($,v0))]
    All(Vec<Goal>),
    CoherenceMode(Goal),
    Ambiguous,
}

impl Goal {
    pub fn ambiguous() -> Self {
        GoalData::Ambiguous.upcast()
    }

    pub fn for_all(names: &[KindedVarIndex], data: impl Upcast<Goal>) -> Self {
        GoalData::ForAll(Binder::new(names, data.upcast())).upcast()
    }

    pub fn for_all_f<A, G>(op: impl FnOnce(A) -> G) -> Goal
    where
        A: Kinded,
        G: Upcast<Goal>,
    {
        let (names, value) = A::instantiate();
        let data = op(value);
        GoalData::ForAll(Binder::new(&names, data.upcast())).upcast()
    }

    pub fn exists(names: &[KindedVarIndex], data: impl Upcast<Goal>) -> Self {
        GoalData::Exists(Binder::new(names, data.upcast())).upcast()
    }

    pub fn exists_f<A, G>(op: impl FnOnce(A) -> G) -> Goal
    where
        A: Kinded,
        G: Upcast<Goal>,
    {
        let (names, value) = A::instantiate();
        let data = op(value);
        GoalData::Exists(Binder::new(&names, data.upcast())).upcast()
    }

    pub fn implies(
        conditions: impl Upcast<Vec<Hypothesis>>,
        consequence: impl Upcast<Goal>,
    ) -> Self {
        GoalData::Implies(conditions.upcast(), consequence.upcast()).upcast()
    }

    /// Goal that is always true.
    pub fn t() -> Self {
        let empty: Vec<Goal> = vec![];
        Self::all(empty)
    }

    /// Goal that is always false.
    pub fn f() -> Self {
        let empty: Vec<Goal> = vec![];
        Self::any(empty)
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

#[term($data)]
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
            HypothesisData::Atomic(APR::AtomicPredicate(p)) => p.debone().0 == predicate.debone().0,
            HypothesisData::Atomic(APR::AtomicRelation(_)) => false,
            HypothesisData::ForAll(binder) => binder.peek().could_match(predicate),
            HypothesisData::Implies(_, consequence) => consequence.could_match(predicate),
        }
    }
}

impl UpcastFrom<HypothesisData> for Hypothesis {
    fn upcast_from(v: HypothesisData) -> Self {
        Hypothesis { data: Arc::new(v) }
    }
}

impl Downcast<HypothesisData> for Hypothesis {
    fn downcast(&self) -> Option<HypothesisData> {
        Some(self.data().clone())
    }
}

/// A set of hypotheses that has been fully elaborated with respect
/// to the invariants in the database.
#[term]
pub struct ElaboratedHypotheses {
    pub set: Set<Hypothesis>,
}

impl ElaboratedHypotheses {
    /// An empty set of hypotheses
    pub fn none() -> Self {
        Self {
            set: Set::default(),
        }
    }
}

impl std::ops::Deref for ElaboratedHypotheses {
    type Target = Set<Hypothesis>;

    fn deref(&self) -> &Self::Target {
        &self.set
    }
}

impl<'a> IntoIterator for &'a ElaboratedHypotheses {
    type Item = &'a Hypothesis;

    type IntoIter = std::collections::btree_set::Iter<'a, Hypothesis>;

    fn into_iter(self) -> Self::IntoIter {
        self.set.iter()
    }
}

#[term]
pub enum HypothesisData {
    #[cast]
    Atomic(APR),
    ForAll(Binder<Hypothesis>),
    Implies(Vec<Goal>, Hypothesis),
}

impl Hypothesis {
    pub fn for_all(names: impl Upcast<Vec<KindedVarIndex>>, data: impl Upcast<Hypothesis>) -> Self {
        let names = names.upcast();
        HypothesisData::ForAll(Binder::new(&names, data.upcast())).upcast()
    }

    pub fn for_all_f<A, G>(op: impl FnOnce(A) -> G) -> Hypothesis
    where
        A: Kinded,
        G: Upcast<Hypothesis>,
    {
        let (names, value) = A::instantiate();
        let data = op(value);
        HypothesisData::ForAll(Binder::new(&names, data.upcast())).upcast()
    }

    pub fn implies(
        conditions: impl Upcast<Vec<Goal>>,
        consequence: impl Upcast<Hypothesis>,
    ) -> Self {
        HypothesisData::Implies(conditions.upcast(), consequence.upcast()).upcast()
    }
}

pub type Hypotheses = Vec<Hypothesis>;

#[term($binder)]
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
            PredicateData::Atomic(a) => a.to(),
            PredicateData::ForAll(binder) => GoalData::ForAll(binder.to()).upcast(),
            PredicateData::Implies(p, q) => GoalData::Implies(p.to(), q.to()).upcast(),
        }
    }
}

impl UpcastFrom<Predicate> for Hypothesis {
    fn upcast_from(value: Predicate) -> Self {
        match value.data() {
            PredicateData::Atomic(a) => a.to(),
            PredicateData::ForAll(binder) => HypothesisData::ForAll(binder.to()).upcast(),
            PredicateData::Implies(p, q) => HypothesisData::Implies(p.to(), q.to()).upcast(),
        }
    }
}

// Transitive casting impls:

cast_impl!((AtomicPredicate) <: (APR) <: (PredicateData));
cast_impl!((AtomicRelation) <: (APR) <: (PredicateData));
cast_impl!((AtomicPredicate) <: (APR) <: (GoalData));
cast_impl!((AtomicRelation) <: (APR) <: (GoalData));
cast_impl!((AtomicPredicate) <: (APR) <: (HypothesisData));
cast_impl!((AtomicRelation) <: (APR) <: (HypothesisData));
cast_impl!((AtomicPredicate) <: (PredicateData) <: (Predicate));
cast_impl!((AtomicRelation) <: (PredicateData) <: (Predicate));
cast_impl!((AtomicPredicate) <: (GoalData) <: (Goal));
cast_impl!((AtomicRelation) <: (GoalData) <: (Goal));
cast_impl!((AtomicPredicate) <: (HypothesisData) <: (Hypothesis));
cast_impl!((AtomicRelation) <: (HypothesisData) <: (Hypothesis));
cast_impl!((APR) <: (PredicateData) <: (Predicate));
cast_impl!((APR) <: (GoalData) <: (Goal));
cast_impl!((APR) <: (HypothesisData) <: (Hypothesis));
