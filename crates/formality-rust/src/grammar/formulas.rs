use formality_core::term;

use formality_core::Upcast;

use super::AliasName;
use super::AliasTy;
use super::Const;
use super::MaySpecBound;
use super::Parameter;
use super::Parameters;
use super::TraitId;
use super::Ty;

pub type Fallible<T> = anyhow::Result<T>;

/// Atomic predicates are the base goals we can try to prove; the rules for proving them
/// are derived (at least in part) based on the Rust source declarations.
///
/// The first few variants are *relations*: built-in goals implemented in custom Rust
/// logic rather than derived from declarations. They are matched more strictly than the
/// other predicates (see the `relation-axiom` rule in `prove_via`), so they can be told
/// apart with [`Skeleton::is_relation`][].
#[term]
pub enum Predicate {
    #[grammar($v0 = $v1)]
    Equals(Parameter, Parameter),

    #[grammar($v0 <: $v1)]
    Sub(Parameter, Parameter),

    #[grammar($v0 : $v1)]
    Outlives(Parameter, Parameter),

    #[grammar(@wf($v0))]
    WellFormed(Parameter),

    /// True if a trait is fully implemented (along with all its where clauses).
    #[cast]
    IsImplemented(TraitRef),

    #[grammar(!$v0)]
    NotImplemented(TraitRef),

    /// `may_spec(WC)`: it is decided whether `WC` holds (`prove::may_spec`).
    /// As a where-clause on a fn, it obliges each caller to decide `WC`.
    #[grammar(@may_spec($v0))]
    MaySpec(MaySpecBound),

    #[cast]
    AliasEq(AliasTy, Ty),

    #[grammar(@WellFormedTraitRef($v0))]
    WellFormedTraitRef(TraitRef),

    /// A trait-ref **is local** if the local crate::types could legally implement it for all
    /// possible instantiations of the variables within.
    ///
    /// Example:
    ///
    /// * `T: SomeLocalTrait` is local for all `T` since the local crate::types could create a blanket impl
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

/// The "skeleton" of an atomic predicate is the kernel that contains
/// nothing unifiable and identifies the kind of predicate.
/// If the skeleton's don't match, they are distinct predicates.
#[term]
pub enum Skeleton {
    IsImplemented(TraitId),
    NotImplemented(TraitId),
    MaySpec(MaySpecBound),
    AliasEq(AliasName),
    WellFormed,
    WellFormedTraitRef(TraitId),
    IsLocal(TraitId),
    ConstHasType,

    Equals,
    Sub,
    Outlives,
}

impl Skeleton {
    /// True if this skeleton belongs to a *relation*, i.e. one of the built-in goals
    /// implemented in custom Rust logic. Relations require their parameters to match
    /// exactly, whereas other predicates only require them to be provably equal.
    pub fn is_relation(&self) -> bool {
        match self {
            Skeleton::Equals | Skeleton::Sub | Skeleton::Outlives | Skeleton::WellFormed => true,
            Skeleton::IsImplemented(_)
            | Skeleton::NotImplemented(_)
            | Skeleton::MaySpec(_)
            | Skeleton::AliasEq(_)
            | Skeleton::WellFormedTraitRef(_)
            | Skeleton::IsLocal(_)
            | Skeleton::ConstHasType => false,
        }
    }
}

impl Predicate {
    /// Separate an atomic predicate into the "skeleton" (which can be compared for equality using `==`)
    /// and the parameters (which must be related).
    #[tracing::instrument(level = "trace", ret)]
    pub fn debone(&self) -> (Skeleton, Vec<Parameter>) {
        match self {
            Predicate::Equals(a, b) => (Skeleton::Equals, vec![a.clone(), b.clone()]),
            Predicate::Sub(a, b) => (Skeleton::Sub, vec![a.clone(), b.clone()]),
            Predicate::Outlives(a, b) => (Skeleton::Outlives, vec![a.clone(), b.clone()]),
            Predicate::WellFormed(p) => (Skeleton::WellFormed, vec![p.clone()]),
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
            // A `may_spec` matches an assumption only if the bound is the same.
            Predicate::MaySpec(bound) => (Skeleton::MaySpec(bound.clone()), vec![]),
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

    pub fn with_self(&self, self_ty: impl Upcast<Ty>) -> TraitRef {
        self.with(self_ty, Vec::<Parameter>::new())
    }
}
