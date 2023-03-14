use anyhow::bail;
use formality_prove::Env;
use formality_rust::grammar::{Crate, TraitImpl};
use formality_types::{
    cast::Downcasted,
    grammar::{Fallible, Wcs},
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

    fn orphan_check(&self, _impl_a: &TraitImpl) -> Fallible<()> {
        Ok(())
    }

    fn overlap_check(&self, impl_a: &TraitImpl, impl_b: &TraitImpl) -> Fallible<()> {
        let mut env = Env::default();

        let a = env.instantiate_universally(&impl_a.binder);
        let b = env.instantiate_universally(&impl_b.binder);

        let trait_ref_a = a.trait_ref();
        let trait_ref_b = b.trait_ref();

        // If the parameters from the two impls cannot be equal, then they do not overlap.
        if let Ok(()) = self.prove_not_goal(
            &env,
            (&a.where_clauses, &b.where_clauses),
            Wcs::all_eq(&trait_ref_a.parameters, &trait_ref_b.parameters),
        ) {
            return Ok(());
        }

        bail!("impls may overlap: `{impl_a:?}` vs `{impl_b:?}`")
    }
}
