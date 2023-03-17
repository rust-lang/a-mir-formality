use anyhow::bail;
use fn_error_context::context;
use formality_prove::Env;
use formality_rust::grammar::{Crate, TraitImpl};
use formality_types::{
    cast::Downcasted,
    grammar::{Fallible, Wc, Wcs},
};
use itertools::Itertools;

use crate::Check;

impl Check<'_> {
    pub(crate) fn check_coherence(&self, current_crate: &Crate) -> Fallible<()> {
        let all_crate_impls: Vec<TraitImpl> =
            self.program.items_from_all_crates().downcasted().collect();
        let current_crate_impls: Vec<TraitImpl> = current_crate.items.iter().downcasted().collect();

        for impl_a in &current_crate_impls {
            self.orphan_check(impl_a)?;
        }

        // check for duplicate impls in the current crate
        for (impl_a, i) in current_crate_impls.iter().zip(0..) {
            if current_crate_impls[i + 1..].contains(impl_a) {
                bail!("duplicate impl in current crate: {:?}", impl_a)
            }
        }

        // check each impl in current crate against impls in all other crates
        for (impl_a, impl_b) in current_crate_impls
            .iter()
            .cartesian_product(&all_crate_impls)
            .filter(|(impl_a, impl_b)| impl_a != impl_b)
        {
            self.overlap_check(impl_a, impl_b)?;
        }

        Ok(())
    }

    #[context("orphan_check({impl_a:?})")]
    fn orphan_check(&self, impl_a: &TraitImpl) -> Fallible<()> {
        let mut env = Env::default();

        let a = env.instantiate_universally(&impl_a.binder);
        let trait_ref = a.trait_ref();

        self.prove_is_local_trait_ref(&env.with_coherence_mode(true), &a.where_clauses, trait_ref)
    }

    #[tracing::instrument(level = "Debug", skip(self))]
    fn overlap_check(&self, impl_a: &TraitImpl, impl_b: &TraitImpl) -> Fallible<()> {
        let mut env = Env::default();

        // Example:
        //
        // Given two impls...
        //
        //   impl<P_a..> SomeTrait<T_a...> for T_a0 where Wc_a { }
        //   impl<P_b..> SomeTrait<T_b...> for T_b0 where Wc_b { }

        // ∀P_a, ∀P_b....
        let a = env.instantiate_universally(&impl_a.binder);
        let b = env.instantiate_universally(&impl_b.binder);

        // ...get the trait refs from each impl...
        let trait_ref_a = a.trait_ref();
        let trait_ref_b = b.trait_ref();

        // If the parameters from the two impls cannot be equal, then they do not overlap.
        //
        // If we can prove `∀P_a, ∀P_b. not (T_a = T_b, Wc_a, Wc_b)`, then they do not overlap.
        if let Ok(()) = self.prove_not_goal(
            &env.with_coherence_mode(true),
            (&a.where_clauses, &b.where_clauses),
            Wcs::all_eq(&trait_ref_a.parameters, &trait_ref_b.parameters),
        ) {
            return Ok(());
        }

        // If we can disprove the where clauses, then they do not overlap.
        //
        // If we can prove `∀P_a, ∀P_b. not [(T_a = T_b) => (Wc_a, Wc_b)]`, then they do not overlap.
        if let Ok(()) = self.prove_not_goal(
            &env.with_coherence_mode(true),
            Wcs::all_eq(&trait_ref_a.parameters, &trait_ref_b.parameters),
            (&a.where_clauses, &b.where_clauses),
        ) {
            return Ok(());
        }

        // If we can disprove the where clauses, then they do not overlap.
        //
        // Given some inverted where-clause Wc_i from (invert(Wc_a), invert(Wc_b))...e.g.
        // if `T: Debug` is in `Wc_a`, then `Wc_i` might be `T: !Debug`.
        //
        // If we can prove `∀P_a, ∀P_b, (T_a = T_b, Wc_a, Wc_b) => Wc_i`, then contradiction, no overlap.
        let inverted: Vec<Wc> = a
            .where_clauses
            .iter()
            .chain(&b.where_clauses)
            .flat_map(|wc| wc.invert())
            .collect();
        if inverted.iter().any(|inverted_wc| {
            self.prove_goal(
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
            return Ok(());
        }
        bail!("impls may overlap: `{impl_a:?}` vs `{impl_b:?}`")
    }
}
