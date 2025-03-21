use std::sync::Arc;

use formality_core::term;

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

    /// A trait-ref **is local** if the local crate could legally implement it for all
    /// possible instantiations of the variables within.
    ///
    /// Example:
    ///
    /// * `T: SomeLocalTrait` is local for all `T` since the local crate could create a blanket impl
    /// * `LocalType<T>: SomeRemoteTrait` is local
    /// * `RemoteType<T>: SomeRemoteTrait` is not local
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
    EffectSubset,

    Equals,
    Sub,
    Outlives,
}

/// A "deboned predicate" is an alternate representation
/// of a [`Predicate`][] where the "constant" parts of the
/// predicate (e.g., which variant is it, the traid-id, etc)
/// are pulled into the "skeleton" and the unifiable parts
/// (types, lifetimes, effects) are pulled into a distinct list.
///
/// This is useful for unifying predicates because you can
/// (1) compare the skeletons with `==` and then (2) unify the rest
/// and you don't have to write a bunch of tedious code to
/// match variants one by one.
#[term]
pub struct DebonedPredicate {
    pub skeleton: Skeleton,
    pub parameters: Vec<Parameter>,
    pub effects: Vec<Effect>,
}

impl Predicate {
    /// Separate an atomic predicate into the "skeleton" (which can be compared for equality using `==`)
    /// and the parameters (which must be related).
    #[tracing::instrument(level = "trace", ret)]
    pub fn debone(&self) -> DebonedPredicate {
        match self {
            Predicate::IsImplemented(TraitRef {
                effect: _,
                trait_id,
                parameters,
            }) => DebonedPredicate {
                skeleton: Skeleton::IsImplemented(trait_id.clone()),
                parameters: parameters.clone(),
                effects: Default::default(),
            },
            Predicate::NotImplemented(TraitRef {
                effect: _,
                trait_id,
                parameters,
            }) => DebonedPredicate {
                skeleton: Skeleton::NotImplemented(trait_id.clone()),
                parameters: parameters.clone(),
                effects: Default::default(),
            },
            Predicate::AliasEq(AliasTy { name, parameters }, ty) => {
                let mut params = parameters.clone();
                params.push(ty.clone().upcast());
                DebonedPredicate {
                    skeleton: Skeleton::AliasEq(name.clone()),
                    parameters: params,
                    effects: Default::default(),
                }
            }
            Predicate::WellFormedTraitRef(TraitRef {
                effect: _,
                trait_id,
                parameters,
            }) => DebonedPredicate {
                skeleton: Skeleton::WellFormedTraitRef(trait_id.clone()),
                parameters: parameters.clone(),
                effects: Default::default(),
            },
            Predicate::IsLocal(TraitRef {
                effect: _,
                trait_id,
                parameters,
            }) => DebonedPredicate {
                skeleton: Skeleton::IsLocal(trait_id.clone()),
                parameters: parameters.clone(),
                effects: Default::default(),
            },
            Predicate::ConstHasType(ct, ty) => DebonedPredicate {
                skeleton: Skeleton::ConstHasType,
                parameters: vec![ct.clone().upcast(), ty.clone().upcast()],
                effects: Default::default(),
            },
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

    /// A trait-ref **is local** if the local crate could legally implement it
    /// (and not via a blanket impl).
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

    // Means that the effects `$v0` are a subset of `$v1`.
    #[grammar(@subset($v0, $v1))]
    EffectSubset(Effect, Effect),
}

impl Relation {
    #[tracing::instrument(level = "trace", ret)]
    pub fn debone(&self) -> DebonedPredicate {
        match self {
            Relation::Equals(a, b) => DebonedPredicate {
                skeleton: Skeleton::Equals,
                parameters: vec![a.clone(), b.clone()],
                effects: Default::default(),
            },
            Relation::Sub(a, b) => DebonedPredicate {
                skeleton: Skeleton::Sub,
                parameters: vec![a.clone(), b.clone()],
                effects: Default::default(),
            },
            Relation::Outlives(a, b) => DebonedPredicate {
                skeleton: Skeleton::Outlives,
                parameters: vec![a.clone(), b.clone()],
                effects: Default::default(),
            },
            Relation::WellFormed(p) => DebonedPredicate {
                skeleton: Skeleton::WellFormed,
                parameters: vec![p.clone()],
                effects: Default::default(),
            },
            Relation::EffectSubset(e1, e2) => DebonedPredicate {
                skeleton: Skeleton::EffectSubset,
                parameters: Default::default(),
                effects: vec![e1, e2].upcast(),
            },
        }
    }
}

#[term]
pub enum Effect {
    #[cast]
    Atomic(AtomicEffect),

    #[grammar(union($v0, $v1))]
    Union(Arc<Effect>, Arc<Effect>),
}

impl Default for Effect {
    fn default() -> Self {
        AtomicEffect::default().upcast()
    }
}

#[term]
#[derive(Default)]
pub enum AtomicEffect {
    Const,
    #[default]
    Runtime,
    // For <T as Trait<..>>::E, TraitRef can uniquely identify an impl, and an impl has only one effect.
    #[grammar(AssociatedEffect($v0))]
    AssociatedEffect(Arc<TraitRef>),
}

#[term($?effect $trait_id ( $,parameters ))]
pub struct TraitRef {
    pub effect: Effect,
    pub trait_id: TraitId,
    pub parameters: Parameters,
}

impl TraitId {
    pub fn with(
        &self,
        effect: &Effect,
        self_ty: impl Upcast<Ty>,
        parameters: impl Upcast<Vec<Parameter>>,
    ) -> TraitRef {
        let self_ty: Ty = self_ty.upcast();
        let parameters: Vec<Parameter> = parameters.upcast();
        TraitRef::new(effect, self, (Some(self_ty), parameters))
    }
}

pub trait Debone {
    fn debone(&self) -> DebonedPredicate;
}

macro_rules! debone_impl {
    ($t:ty) => {
        impl Debone for $t {
            fn debone(&self) -> DebonedPredicate {
                self.debone()
            }
        }
    };
}

debone_impl!(Predicate);
debone_impl!(Relation);
