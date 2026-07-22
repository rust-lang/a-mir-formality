//! This crate contains the trait proving + type inference logic.
//! It correpsonds loosely to the `InferenceContext` and trait solving (fulfillment context, etc)
//! in the Rust compiler.
//!
//! The base operations we export are:
//!
//! * [`prove`][] -- prove a set of where-clauses to be true
//! * [`prove_normalize`][] -- normalize a type one step (typically used in a recursive setup)

use crate::grammar::{Binder, Crates, Predicate, Relation, Ty, Wc, Wcs, WhereBound, WhereClause};
use crate::rust::FormalityLang;
use formality_core::judgment::{EachProof, FailedRule, FailureLocation, ProofTree};
use formality_core::visit::CoreVisit;
use formality_core::{map, set, ProvenSet, Upcast};
use std::sync::Arc;
use tracing::Level;

pub mod combinators;
mod constraints;
mod db;
mod decls;
mod env;
mod is_local;
mod minimize;
mod negation;
mod prove_after;
mod prove_const_has_type;
mod prove_eq;
pub mod prove_normalize;
mod prove_outlives;
mod prove_sub;
mod prove_via;
mod prove_wc;
mod prove_wc_list;
mod prove_wf;
pub mod test_util;

#[cfg(test)]
mod test;

pub use constraints::{Constrained, Constraints};
pub use decls::*;
pub use env::{Bias, Env, MaxUniverse, Universe};
pub use negation::{is_definitely_not_proveable, may_not_be_provable, negation_via_failure};
pub use prove_normalize::prove_normalize;

use self::prove_wc_list::prove_wc_list;

impl Crates {
    pub fn to_prove_decls(&self) -> Program {
        Program {
            crates: Arc::new(self.clone()),
            max_size: Program::DEFAULT_MAX_SIZE,
        }
    }
}

/// Top-level entry point for proving things; other rules recurse to this one.
#[track_caller]
pub fn prove(
    decls: impl Upcast<Program>,
    env: impl Upcast<Env>,
    assumptions: impl Upcast<Wcs>,
    goal: impl Upcast<Wcs>,
) -> ProvenSet<Constraints> {
    let decls: Program = decls.upcast();
    let env: Env = env.upcast();
    let assumptions: Wcs = assumptions.upcast();
    let goal: Wcs = goal.upcast();

    // "Minimize" the env/assumptions/goals so that we better detect cycles.
    let (env, (assumptions, goal), min) = minimize::minimize(env, (assumptions, goal));

    // Establish context for debugging/tracing logs.
    let span = tracing::span!(Level::DEBUG, "prove", ?goal, ?assumptions, ?env, ?decls);
    let _guard = span.enter();

    // Fail if the terms are getting too large ("overflow detection").
    // This is meant to capture complex recursion cycles that will never terminate but also
    // never reach a (simple) cycle, e.g., proving `A: Foo` requires proving `Vec<A>: Foo`
    // requires proving `Vec<Vec<A>>: Foo` etc.
    //
    // In the compiler we use recursion depth instead. We avoid recursion depth because it requires
    // knowing the context in which the proof occurs.
    let term_in = (&assumptions, &goal);
    if term_in.size() > decls.max_size {
        tracing::debug!(
            "term has size {} which exceeds max size of {}",
            term_in.size(),
            decls.max_size
        );
        return ProvenSet::singleton((
            Constraints::none(env).ambiguous(),
            ProofTree::leaf("max term size exceeded"),
        ));
    }

    // Assert the term we are trying to prove should not have any variables that are not in the environment.
    assert!(env.encloses(term_in));

    // Call `prove_wc_list` to do the real work.
    struct ProveFailureLabel(String);
    let label = ProveFailureLabel(format!(
        "prove {{ goal: {goal:?}, assumptions: {assumptions:?}, env: {env:?}, decls: {decls:?} }}"
    ));
    impl std::fmt::Debug for ProveFailureLabel {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str(&self.0)
        }
    }
    let mut results = map![];
    let result_set = if let Err(e) =
        prove_wc_list(decls, &env, assumptions, goal).each_proof(|(result, proof_tree)| {
            results.insert(result, proof_tree);
        }) {
        ProvenSet::failed_rules(label, FailureLocation::caller(), set![FailedRule::new(e)])
    } else {
        ProvenSet::proven(results)
    };

    tracing::debug!(?result_set);

    // Map the results back to the "unminimized" form ("reconstitute").
    let maxified = result_set.map(|(r, proof_tree)| {
        assert!(r.is_valid_extension_of(&env));
        (min.reconstitute(r), proof_tree)
    });

    tracing::debug!(?maxified);

    maxified
}

pub trait ToWcs {
    fn to_wcs(&self) -> Wcs;
}

impl<T: ?Sized + ToWcs> ToWcs for &T {
    fn to_wcs(&self) -> Wcs {
        T::to_wcs(self)
    }
}

macro_rules! upcast_to_wcs {
    ($($t:ty,)*) => {
        $(
            impl ToWcs for $t {
                fn to_wcs(&self) -> Wcs {
                    self.upcast()
                }
            }
        )*
    }
}

upcast_to_wcs! {
    Wc,
    Wcs,
    Predicate,
    Relation,
}

impl ToWcs for () {
    fn to_wcs(&self) -> Wcs {
        Wcs::t()
    }
}

impl<A, B> ToWcs for (A, B)
where
    A: ToWcs,
    B: ToWcs,
{
    fn to_wcs(&self) -> Wcs {
        let (a, b) = self;
        let a = a.to_wcs();
        let b = b.to_wcs();
        (a, b).upcast()
    }
}

impl<A, B, C> ToWcs for (A, B, C)
where
    A: ToWcs,
    B: ToWcs,
    C: ToWcs,
{
    fn to_wcs(&self) -> Wcs {
        let (a, b, c) = self;
        let a = a.to_wcs();
        let b = b.to_wcs();
        let c = c.to_wcs();
        (a, b, c).upcast()
    }
}

impl ToWcs for Vec<WhereClause> {
    fn to_wcs(&self) -> Wcs {
        self.iter().flat_map(|wc| wc.to_wcs()).collect()
    }
}

impl ToWcs for [WhereClause] {
    fn to_wcs(&self) -> Wcs {
        self.iter().flat_map(|wc| wc.to_wcs()).collect()
    }
}

impl ToWcs for WhereClause {
    fn to_wcs(&self) -> Wcs {
        match self {
            WhereClause::IsImplemented(self_ty, trait_id, parameters) => {
                trait_id.with(self_ty, parameters).upcast()
            }
            WhereClause::AliasEq(alias_ty, ty) => {
                Predicate::AliasEq(alias_ty.clone(), ty.clone()).upcast()
            }
            WhereClause::Outlives(a, b) => Relation::outlives(a, b).upcast(),
            WhereClause::ForAll(binder) => {
                let (vars, wc) = binder.open();
                wc.to_wcs()
                    .into_iter()
                    .map(|wc| Wc::for_all(Binder::new(&vars, wc)))
                    .collect()
            }
            WhereClause::TypeOfConst(ct, ty) => {
                Predicate::ConstHasType(ct.clone(), ty.clone()).upcast()
            }
        }
    }
}

impl WhereBound {
    pub fn to_wc(&self, self_ty: impl Upcast<Ty>) -> Wc {
        let self_ty: Ty = self_ty.upcast();

        match self {
            WhereBound::IsImplemented(trait_id, parameters) => {
                trait_id.with(self_ty, parameters).upcast()
            }
            WhereBound::Outlives(lt) => Relation::outlives(self_ty, lt).upcast(),
            WhereBound::ForAll(binder) => {
                let (vars, bound) = binder.open();
                Wc::for_all(Binder::new(&vars, bound.to_wc(self_ty)))
            }
        }
    }
}
