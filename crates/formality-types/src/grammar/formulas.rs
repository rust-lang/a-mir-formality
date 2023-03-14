use formality_macros::term;

use crate::cast::To;
use crate::cast::Upcast;
use crate::cast_impl;

use super::AdtId;
use super::AliasName;
use super::AliasTy;
use super::Parameter;
use super::Parameters;
use super::TraitId;
use super::Ty;

pub type Fallible<T> = anyhow::Result<T>;

/// Atomic predicates are the base goals we can try to prove; the rules for proving them
/// are derived (at least in part) based on the Rust source declarations.
#[term]
pub enum Predicate {
    /// True if a trait is fully implemented (along with all its where clauses).
    #[cast]
    IsImplemented(TraitRef),

    /// True if an alias normalizes to the given type.
    #[grammar(@NormalizesTo($v0 -> $v1))]
    NormalizesTo(AliasTy, Ty),

    #[grammar(@WellFormedAdt($v0, $v1))]
    WellFormedAdt(AdtId, Parameters),

    #[grammar(@WellFormedAlias($v0, $v1))]
    WellFormedAlias(AliasName, Parameters),

    #[grammar(@WellFormedTraitRef($v0))]
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

impl Predicate {
    /// True if this goal can be proven via a cycle. For example,
    /// it is ok for a `T: Debug` impl to require `T: Debug`.
    pub fn is_coinductive(&self) -> Coinductive {
        match self {
            Predicate::IsImplemented(_) => Coinductive::Yes,
            Predicate::NormalizesTo(_, _) => Coinductive::No,
            Predicate::WellFormedAlias(_, _)
            | Predicate::WellFormedAdt(_, _)
            | Predicate::WellFormedTraitRef(_) => Coinductive::Yes,
        }
    }
}

impl AliasName {
    pub fn well_formed(&self, t: impl Upcast<Vec<Parameter>>) -> Predicate {
        Predicate::WellFormedAlias(self.clone(), t.upcast())
    }
}

impl AliasTy {
    pub fn normalizes_to(&self, t: impl Upcast<Ty>) -> Predicate {
        Predicate::NormalizesTo(self.clone(), t.upcast())
    }
}

impl Ty {
    pub fn well_formed(&self) -> AtomicRelation {
        AtomicRelation::WellFormed(self.upcast())
    }
}

impl Parameter {
    /// Well-formed goal for a parameter
    pub fn well_formed(&self) -> AtomicRelation {
        AtomicRelation::WellFormed(self.to())
    }

    pub fn outlives(&self, b: impl Upcast<Parameter>) -> AtomicRelation {
        AtomicRelation::Outlives(self.clone(), b.upcast())
    }
}

/// The "skeleton" of an atomic predicate is the kernel that contains
/// nothing unifiable and identifies the kind of predicate.
/// If the skeleton's don't match, they are distinct predicates.
#[term]
pub enum Skeleton {
    IsImplemented(TraitId),
    NormalizesTo(AliasName),
    WellFormed,
    WellFormedAdt(AdtId),
    WellFormedAlias(AliasName),
    WellFormedTraitRef(TraitId),

    Equals,
    Sub,
    Outlives,
}

impl Predicate {
    /// Separate an atomic predicate into the "skeleton" (which can be compared for equality using `==`)
    /// and the parameters (which must be related).
    pub fn debone(&self) -> (Skeleton, Vec<Parameter>) {
        match self {
            Predicate::IsImplemented(TraitRef {
                trait_id,
                parameters,
            }) => (
                Skeleton::IsImplemented(trait_id.clone()),
                parameters.clone(),
            ),
            Predicate::NormalizesTo(AliasTy { name, parameters }, ty) => (
                Skeleton::NormalizesTo(name.clone()),
                parameters
                    .iter()
                    .cloned()
                    .chain(Some(ty.to_parameter()))
                    .collect(),
            ),
            Predicate::WellFormedAdt(id, parameters) => {
                (Skeleton::WellFormedAdt(id.clone()), parameters.clone())
            }
            Predicate::WellFormedAlias(id, parameters) => {
                (Skeleton::WellFormedAlias(id.clone()), parameters.clone())
            }
            Predicate::WellFormedTraitRef(TraitRef {
                trait_id,
                parameters,
            }) => (
                Skeleton::WellFormedTraitRef(trait_id.clone()),
                parameters.clone(),
            ),
        }
    }
}

impl TraitRef {
    pub fn is_implemented(&self) -> Predicate {
        Predicate::IsImplemented(self.clone())
    }

    pub fn well_formed(&self) -> Predicate {
        Predicate::WellFormedTraitRef(self.clone())
    }
}

impl AdtId {
    pub fn well_formed(&self, parameters: impl Upcast<Vec<Parameter>>) -> Predicate {
        Predicate::WellFormedAdt(self.clone(), parameters.upcast())
    }
}

/// Relations are built-in goals which are implemented in custom Rust logic.
#[term]
pub enum AtomicRelation {
    #[grammar($v0 = $v1)]
    Equals(Parameter, Parameter),

    #[grammar($v0 <: $v1)]
    Sub(Parameter, Parameter),

    #[grammar($v0 : $v1)]
    Outlives(Parameter, Parameter),

    #[grammar(@wf($v0))]
    WellFormed(Parameter),
}

impl AtomicRelation {
    /// Capture a few trivial cases; we screen these out to cleanup the results
    /// from queries.
    pub fn is_trivially_true(&self) -> bool {
        match self {
            AtomicRelation::Equals(a, b) => a == b,
            AtomicRelation::Sub(a, b) => a == b,
            AtomicRelation::Outlives(a, b) => a == b,
            AtomicRelation::WellFormed(_) => false,
        }
    }

    pub fn eq(p1: impl Upcast<Parameter>, p2: impl Upcast<Parameter>) -> Self {
        Self::Equals(p1.upcast(), p2.upcast())
    }

    pub fn outlives(p1: impl Upcast<Parameter>, p2: impl Upcast<Parameter>) -> Self {
        Self::Outlives(p1.upcast(), p2.upcast())
    }

    pub fn sub(p1: impl Upcast<Parameter>, p2: impl Upcast<Parameter>) -> Self {
        Self::Sub(p1.upcast(), p2.upcast())
    }

    pub fn debone(&self) -> (Skeleton, Vec<Parameter>) {
        match self {
            AtomicRelation::Equals(a, b) => (Skeleton::Equals, vec![a.clone(), b.clone()]),
            AtomicRelation::Sub(a, b) => (Skeleton::Sub, vec![a.clone(), b.clone()]),
            AtomicRelation::Outlives(a, b) => (Skeleton::Outlives, vec![a.clone(), b.clone()]),
            AtomicRelation::WellFormed(p) => (Skeleton::WellFormed, vec![p.clone()]),
        }
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

impl TraitId {
    pub fn with(
        &self,
        self_ty: impl Upcast<Ty>,
        parameters: impl Upcast<Vec<Parameter>>,
    ) -> TraitRef {
        let self_ty: Ty = self_ty.upcast();
        let parameters: Vec<Parameter> = parameters.upcast();
        TraitRef::new(self, (Some(self_ty), parameters))
    }
}

/// "APR" == AtomicPredicateOrRelation
///
/// We need a better name for this lol.
#[term]
pub enum APR {
    #[cast]
    AtomicPredicate(Predicate),
    #[cast]
    AtomicRelation(AtomicRelation),
}

impl APR {
    pub fn debone(&self) -> (Skeleton, Vec<Parameter>) {
        match self {
            APR::AtomicPredicate(v) => v.debone(),
            APR::AtomicRelation(v) => v.debone(),
        }
    }
}

pub trait Debone {
    fn debone(&self) -> (Skeleton, Vec<Parameter>);
}

macro_rules! debone_impl {
    ($t:ty) => {
        impl Debone for $t {
            fn debone(&self) -> (Skeleton, Vec<Parameter>) {
                self.debone()
            }
        }
    };
}

debone_impl!(APR);
debone_impl!(Predicate);
debone_impl!(AtomicRelation);

// Transitive casting impls:

cast_impl!((TraitRef) <: (Predicate) <: (APR));
