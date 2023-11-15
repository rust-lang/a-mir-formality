use formality_core::term;

use formality_core::cast_impl;
use formality_core::To;
use formality_core::Upcast;

use super::AliasName;
use super::AliasTy;
use super::Const;
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

    #[grammar(!$v0)]
    NotImplemented(TraitRef),

    #[cast]
    AliasEq(AliasTy, Ty),

    #[grammar(@WellFormedTraitRef($v0))]
    WellFormedTraitRef(TraitRef),

    #[grammar(@IsLocal($v0))]
    IsLocal(TraitRef),

    #[grammar(@ConstHasType($v0, $v1))]
    ConstHasType(Const, Ty),
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

impl TraitRef {
    pub fn not_implemented(&self) -> Predicate {
        Predicate::NotImplemented(self.clone())
    }
}

impl Ty {
    pub fn well_formed(&self) -> Relation {
        Relation::WellFormed(self.upcast())
    }
}

impl Parameter {
    /// Well-formed goal for a parameter
    pub fn well_formed(&self) -> Relation {
        Relation::WellFormed(self.to())
    }

    pub fn outlives(&self, b: impl Upcast<Parameter>) -> Relation {
        Relation::Outlives(self.clone(), b.upcast())
    }
}

/// The "skeleton" of an atomic predicate is the kernel that contains
/// nothing unifiable and identifies the kind of predicate.
/// If the skeleton's don't match, they are distinct predicates.
#[term]
pub enum Skeleton {
    IsImplemented(TraitId),
    NotImplemented(TraitId),
    AliasEq(AliasName),
    WellFormed,
    WellFormedTraitRef(TraitId),
    IsLocal(TraitId),
    ConstHasType,

    Equals,
    Sub,
    Outlives,
}

impl Predicate {
    /// Separate an atomic predicate into the "skeleton" (which can be compared for equality using `==`)
    /// and the parameters (which must be related).
    #[tracing::instrument(level = "trace", ret)]
    pub fn debone(&self) -> (Skeleton, Vec<Parameter>) {
        match self {
            Predicate::IsImplemented(TraitRef {
                trait_id,
                parameters,
            }) => (
                Skeleton::IsImplemented(trait_id.clone()),
                parameters.clone(),
            ),
            Predicate::NotImplemented(TraitRef {
                trait_id,
                parameters,
            }) => (
                Skeleton::NotImplemented(trait_id.clone()),
                parameters.clone(),
            ),
            Predicate::AliasEq(AliasTy { name, parameters }, ty) => {
                let mut params = parameters.clone();
                params.push(ty.clone().upcast());
                (Skeleton::AliasEq(name.clone()), params)
            }
            Predicate::WellFormedTraitRef(TraitRef {
                trait_id,
                parameters,
            }) => (
                Skeleton::WellFormedTraitRef(trait_id.clone()),
                parameters.clone(),
            ),
            Predicate::IsLocal(TraitRef {
                trait_id,
                parameters,
            }) => (Skeleton::IsLocal(trait_id.clone()), parameters.clone()),
            Predicate::ConstHasType(ct, ty) => (
                Skeleton::ConstHasType,
                vec![ct.clone().upcast(), ty.clone().upcast()],
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

    pub fn is_local(&self) -> Predicate {
        Predicate::IsLocal(self.clone())
    }
}

/// Relations are built-in goals which are implemented in custom Rust logic.
#[term]
pub enum Relation {
    #[grammar($v0 = $v1)]
    Equals(Parameter, Parameter),

    #[grammar($v0 <: $v1)]
    Sub(Parameter, Parameter),

    #[grammar($v0 : $v1)]
    Outlives(Parameter, Parameter),

    #[grammar(@wf($v0))]
    WellFormed(Parameter),
}

impl Relation {
    #[tracing::instrument(level = "trace", ret)]
    pub fn debone(&self) -> (Skeleton, Vec<Parameter>) {
        match self {
            Relation::Equals(a, b) => (Skeleton::Equals, vec![a.clone(), b.clone()]),
            Relation::Sub(a, b) => (Skeleton::Sub, vec![a.clone(), b.clone()]),
            Relation::Outlives(a, b) => (Skeleton::Outlives, vec![a.clone(), b.clone()]),
            Relation::WellFormed(p) => (Skeleton::WellFormed, vec![p.clone()]),
        }
    }
}

#[term($trait_id ( $,parameters ))]
pub struct TraitRef {
    pub trait_id: TraitId,
    pub parameters: Parameters,
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

/// "PR" == Predicate or Relation
///
/// We need a better name for this lol.
#[term]
pub enum PR {
    #[cast]
    Predicate(Predicate),
    #[cast]
    Relation(Relation),
}

impl PR {
    pub fn debone(&self) -> (Skeleton, Vec<Parameter>) {
        match self {
            PR::Predicate(v) => v.debone(),
            PR::Relation(v) => v.debone(),
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

debone_impl!(PR);
debone_impl!(Predicate);
debone_impl!(Relation);

// Transitive casting impls:

cast_impl!((TraitRef) <: (Predicate) <: (PR));
