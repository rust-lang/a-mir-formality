use crate::grammar::{
    Crate, Fallible, NegTraitImpl, NegTraitImplBoundData, TraitImpl, TraitImplBoundData, Wc, Wcs,
};
use crate::prove::prove::{Env, Program};
use anyhow::bail;

use super::{prove_goal, prove_not_goal};
use formality_core::{judgment::ProofTree, judgment_fn};

judgment_fn! {
    /// Runs coherence checking (orphan, overlap, duplicates) for the current crate.
    /// Returns ProvenSet<()> caller.
    pub(crate) fn check_coherence(program: Program, current_crate: Crate) => () {
        debug(program, current_crate)

        (
            (let current_crate_impls = program.trait_impls_in_crate(&current_crate))
            (ensure_no_duplicate_impls(current_crate_impls) => ())
            (let all_crate_impls = program.trait_impls())
            (for_all(impl_a in current_crate_impls)
                (for_all(impl_b in all_crate_impls)
                    (overlap_check_impl(program, impl_a, impl_b) => ())
                )
            )
            (for_all(trait_impl in current_crate_impls)
                (orphan_check(program, trait_impl) => ()))
            (for_all(neg_trait_impl in program.neg_trait_impls_in_crate(&current_crate))
                (orphan_check_neg(program, neg_trait_impl) => ()))
            --- ("check_coherence")
            (check_coherence(program, current_crate) => ())
        )
    }
}

// Orphan rule (RFC 2451): prove the trait ref is local under the impl's where-clauses.
judgment_fn! {
    fn orphan_check(program: Program, impl_a: TraitImpl) => () {
        debug(program, impl_a)

        (
            (let (env, a) = open_trait_impl(&impl_a))
            (let trait_ref = a.trait_ref())
            (prove_goal(program, env, &a.where_clauses, trait_ref.is_local()) => ())
            --- ("orphan_check")
            (orphan_check(program, impl_a) => ())
        )
    }
}

judgment_fn! {
    fn orphan_check_neg(program: Program, impl_a: NegTraitImpl) => () {
        debug(program, impl_a)

        (
            (let (env, a) = open_neg_trait_impl(&impl_a))
            (let trait_ref = a.trait_ref())
            (prove_goal(program, env, &a.where_clauses, trait_ref.is_local()) => ())
            --- ("orphan_check_neg")
            (orphan_check_neg(program, impl_a) => ())
        )
    }
}

/// Reject duplicate trait impls in the current crate so overlap pairing cannot match an impl with itself.
fn ensure_no_duplicate_impls(impls: &[TraitImpl]) -> Fallible<ProofTree> {
    for (i, impl_a) in impls.iter().enumerate() {
        if impls[i + 1..].contains(impl_a) {
            bail!("duplicate impl in current crate: {:?}", impl_a);
        }
    }
    Ok(ProofTree::leaf("no duplicate impls"))
}

/// Two impls of the same trait prove they do not overlap or report impls may overlap.
/// Same impl or different trait -> trivial proof .
#[tracing::instrument(level = "Debug", skip(program, impl_a, impl_b))]
fn overlap_check_impl(
    program: &Program,
    impl_a: &TraitImpl,
    impl_b: &TraitImpl,
) -> Fallible<ProofTree> {
    // An impl cannot overlap with itself.
    if impl_a == impl_b {
        return Ok(ProofTree::new(
            "overlap_check",
            Some("skip_same_impl"),
            vec![],
        ));
    }
    // Impls of two distinct traits cannot overlap.
    if impl_a.trait_id() != impl_b.trait_id() {
        return Ok(ProofTree::new(
            "overlap_check",
            Some("skip_different_trait"),
            vec![],
        ));
    }

    // Example:
    //
    // Given two impls...
    //
    //   impl<P_a..> SomeTrait<T_a...> for T_a0 where Wc_a { }
    //   impl<P_b..> SomeTrait<T_b...> for T_b0 where Wc_b { }
    //
    // We want to prove that ∀P_a, ∀P_b ...
    let (env, a) = Env::default().instantiate_universally(&impl_a.binder);
    let (env, b) = env.instantiate_universally(&impl_b.binder);

    let trait_ref_a = a.trait_ref();
    let trait_ref_b = b.trait_ref();

    assert_eq!(trait_ref_a.trait_id, trait_ref_b.trait_id);

    // ... ¬(coherence_mode => (Ts_a = Ts_b ∧ Wc_a ∧ Wc_b))
    //
    // i.e., there is no overlap if, in coherence mode, if we can prove that either
    // * the parameters cannot be equated ¬(Ts_a = Ts_b)
    // * or the where-clauses don't hold (¬Wc_a || ¬Wc_b).
    //
    // TODO: feels like we do want a general "not goal", flipping existentials
    // and universals and the coherence mode.
    // self.prove_not_goal(&env, &(Wcs::wf))
    if let Ok(proof_tree) = prove_not_goal(
        program,
        &env,
        (),
        (
            Wcs::all_eq(&trait_ref_a.parameters, &trait_ref_b.parameters),
            &a.where_clauses,
            &b.where_clauses,
        ),
    ) {
        tracing::debug!(
            "proved not {:?}",
            (
                Wcs::all_eq(&trait_ref_a.parameters, &trait_ref_b.parameters),
                &a.where_clauses,
                &b.where_clauses,
            )
        );

        return Ok(ProofTree::new(
            "overlap_check",
            Some("not_goal"),
            vec![proof_tree],
        ));
    }

    // try inverted where-clauses from Wc_a / Wc_b (e.g. T: Debug => T: !Debug).
    // If (equal params ∧ Wc_a ∧ Wc_b) => Wc_i is provable the two impls cannot both apply.
    let inverted: Vec<Wc> = a
        .where_clauses
        .iter()
        .chain(&b.where_clauses)
        .flat_map(|wc| wc.invert())
        .collect();

    if let Some(inverted_wc) = inverted.iter().find(|inverted_wc| {
        prove_goal(
            program,
            &env,
            (
                Wcs::all_eq(&trait_ref_a.parameters, &trait_ref_b.parameters),
                &a.where_clauses,
                &b.where_clauses,
            ),
            inverted_wc,
        )
        .is_ok()
    }) {
        tracing::debug!(
            "proved {:?} assuming {:?}",
            inverted_wc,
            (
                Wcs::all_eq(&trait_ref_a.parameters, &trait_ref_b.parameters),
                &a.where_clauses,
                &b.where_clauses,
            )
        );

        return Ok(ProofTree::new("overlap_check", Some("inverted"), vec![]));
    }

    bail!("impls may overlap:\n{impl_a:?}\n{impl_b:?}")
}

/// Binder opening via universal_substitution so judgment rules need not use &mut Env.
fn open_trait_impl(impl_a: &TraitImpl) -> (Env, TraitImplBoundData) {
    Env::default().instantiate_universally(&impl_a.binder)
}

/// Same pattern as open_trait_impl for negative impls.
fn open_neg_trait_impl(impl_a: &NegTraitImpl) -> (Env, NegTraitImplBoundData) {
    Env::default().instantiate_universally(&impl_a.binder)
}
