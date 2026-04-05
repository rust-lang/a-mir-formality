use crate::grammar::{
    Crate, Fallible, NegTraitImpl, NegTraitImplBoundData, TraitImpl, TraitImplBoundData, Wc, Wcs,
};
use crate::prove::prove::{Env, Program};
use anyhow::bail;

use super::{prove_goal, prove_not_goal};
use formality_core::{judgment::ProofTree, judgment_fn, ProvenSet};

/// Runs coherence checking (orphan, overlap, duplicates) for the current crate.
/// Returns ProvenSet<()> caller.
pub(crate) fn check_coherence(program: &Program, current_crate: &Crate) -> ProvenSet<()> {
    check_coherence_judgment(program.clone(), current_crate.clone())
}

judgment_fn! {
    fn check_coherence_judgment(program: Program, current_crate: Crate) => () {
        debug(program, current_crate)

        (
            (let current_crate_impls: Vec<TraitImpl> = trait_impls_in_crate(current_crate))
            (let _ = ensure_no_duplicate_impls(&current_crate_impls)?)
            (let all_crate_impls: Vec<TraitImpl> = all_trait_impls(&program))
            (for_all(i in 0..current_crate_impls.len())
                (for_all(j in 0..all_crate_impls.len())
                    (let _overlap: ProofTree = overlap_check_impl(
                        &program,
                        &current_crate_impls[i],
                        &all_crate_impls[j],
                    )?)
                )
            )
            (for_all(idx in 0..current_crate_impls.len())
                (orphan_check(program.clone(), current_crate_impls[idx].clone()) => ()))
            (for_all(impl_a in neg_trait_impls_in_crate(current_crate))
                (orphan_check_neg(program.clone(), impl_a) => ()))
            --- ("check_coherence")
            (check_coherence_judgment(program, current_crate) => ())
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
fn ensure_no_duplicate_impls(impls: &[TraitImpl]) -> Fallible<()> {
    for (i, impl_a) in impls.iter().enumerate() {
        if impls[i + 1..].contains(impl_a) {
            bail!("duplicate impl in current crate: {:?}", impl_a);
        }
    }
    Ok(())
}

/// Two impls of the same trait prove they do not overlap or report impls may overlap.
/// Same impl or different trait -> trivial proof .
#[tracing::instrument(level = "Debug", skip(program, impl_a, impl_b))]
fn overlap_check_impl(
    program: &Program,
    impl_a: &TraitImpl,
    impl_b: &TraitImpl,
) -> Fallible<ProofTree> {
    if impl_a == impl_b {
        return Ok(ProofTree::new(
            "overlap_check",
            Some("skip_same_impl"),
            vec![],
        ));
    }
    if impl_a.trait_id() != impl_b.trait_id() {
        return Ok(ProofTree::new(
            "overlap_check",
            Some("skip_different_trait"),
            vec![],
        ));
    }

    // ∀P_a, ∀P_b — e.g. impl<P_a..> Tr<T_a…> for T_a0 where Wc_a vs same for b.
    let (env, a) = Env::default().instantiate_universally(&impl_a.binder);
    let (env, b) = env.instantiate_universally(&impl_b.binder);

    let trait_ref_a = a.trait_ref();
    let trait_ref_b = b.trait_ref();

    assert_eq!(trait_ref_a.trait_id, trait_ref_b.trait_id);

    // ∀P_a, ∀P_b. ¬(coherence_mode => (Ts_a = Ts_b ∧ Wc_a ∧ Wc_b)) — then no overlap.
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

fn trait_impls_in_crate(krate: &Crate) -> Vec<TraitImpl> {
    krate.items.iter().downcasted().collect()
}

fn neg_trait_impls_in_crate(krate: &Crate) -> Vec<NegTraitImpl> {
    krate.items.iter().downcasted().collect()
}

/// Binder opening via universal_substitution so judgment rules need not use &mut Env.
fn open_trait_impl(impl_a: &TraitImpl) -> (Env, TraitImplBoundData) {
    let (env, subst) = Env::default().universal_substitution(&impl_a.binder);
    let a = impl_a.binder.instantiate_with(&subst).unwrap();
    (env, a)
}

/// Same pattern as open_trait_impl for negative impls.
fn open_neg_trait_impl(impl_a: &NegTraitImpl) -> (Env, NegTraitImplBoundData) {
    let (env, subst) = Env::default().universal_substitution(&impl_a.binder);
    let a = impl_a.binder.instantiate_with(&subst).unwrap();
    (env, a)
}

/// All TraitImpl`s visible across crates.
fn all_trait_impls(program: &Program) -> Vec<TraitImpl> {
    program
        .program()
        .items_from_all_crates()
        .downcasted()
        .collect()
}

