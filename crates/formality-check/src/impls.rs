use anyhow::bail;

use fn_error_context::context;
use formality_core::Downcasted;
use formality_prove::{Env, Safety};
use formality_rust::{
    grammar::{
        AssociatedTy, AssociatedTyBoundData, AssociatedTyValue, AssociatedTyValueBoundData, Fn,
        FnBoundData, ImplItem, NegTraitImpl, NegTraitImplBoundData, Trait, TraitBoundData,
        TraitImpl, TraitImplBoundData, TraitItem,
    },
    prove::ToWcs,
};
use formality_types::{
    grammar::{Binder, Fallible, Relation, Substitution, Wcs},
    rust::Term,
};

impl super::Check<'_> {
    #[context("check_trait_impl({trait_impl:?})")]
    pub(super) fn check_trait_impl(&self, trait_impl: &TraitImpl) -> Fallible<()> {
        let TraitImpl { binder, safety: _ } = trait_impl;

        let mut env = Env::default();

        let TraitImplBoundData {
            trait_id,
            self_ty,
            trait_parameters,
            where_clauses,
            impl_items,
        } = env.instantiate_universally(binder);

        let trait_ref = trait_id.with(self_ty, trait_parameters);

        self.prove_where_clauses_well_formed(&env, &where_clauses, &where_clauses)?;

        self.prove_goal(&env, &where_clauses, trait_ref.is_implemented())?;

        self.prove_not_goal(&env, &where_clauses, trait_ref.not_implemented())?;

        let trait_decl = self.program.trait_named(&trait_ref.trait_id)?;
        let TraitBoundData {
            where_clauses: _,
            trait_items,
        } = trait_decl.binder.instantiate_with(&trait_ref.parameters)?;

        self.check_safety_matches(trait_decl, trait_impl)?;

        for impl_item in &impl_items {
            self.check_trait_impl_item(&env, &where_clauses, &trait_items, impl_item)?;
        }

        Ok(())
    }

    #[context("check_neg_trait_impl({trait_impl:?})")]
    pub(super) fn check_neg_trait_impl(&self, trait_impl: &NegTraitImpl) -> Fallible<()> {
        let NegTraitImpl { binder, safety } = trait_impl;

        let mut env = Env::default();

        let NegTraitImplBoundData {
            trait_id,
            self_ty,
            trait_parameters,
            where_clauses,
        } = env.instantiate_universally(binder);

        let trait_ref = trait_id.with(self_ty, trait_parameters);

        // Negative impls are always safe (rustc E0198) regardless of the trait's safety.
        if *safety == Safety::Unsafe {
            bail!("negative impls cannot be unsafe");
        }

        self.prove_where_clauses_well_formed(&env, &where_clauses, &where_clauses)?;

        self.prove_goal(&env, &where_clauses, trait_ref.not_implemented())?;

        Ok(())
    }

    /// Validate that the declared safety of an impl matches the one from the trait declaration.
    fn check_safety_matches(&self, trait_decl: &Trait, trait_impl: &TraitImpl) -> Fallible<()> {
        if trait_decl.safety != trait_impl.safety {
            match trait_decl.safety {
                Safety::Safe => bail!("implementing the trait `{:?}` is not unsafe", trait_decl.id),
                Safety::Unsafe => bail!(
                    "the trait `{:?}` requires an `unsafe impl` declaration",
                    trait_decl.id
                ),
            }
        }
        Ok(())
    }

    fn check_trait_impl_item(
        &self,
        env: &Env,
        assumptions: impl ToWcs,
        trait_items: &[TraitItem],
        impl_item: &ImplItem,
    ) -> Fallible<()> {
        let assumptions: Wcs = assumptions.to_wcs();
        assert!(
            env.only_universal_variables() && env.encloses((&assumptions, trait_items, impl_item))
        );

        match impl_item {
            ImplItem::Fn(v) => self.check_fn_in_impl(env, &assumptions, trait_items, v),
            ImplItem::AssociatedTyValue(v) => {
                self.check_associated_ty_value(env, assumptions, trait_items, v)
            }
        }
    }

    fn check_fn_in_impl(
        &self,
        env: &Env,
        impl_assumptions: impl ToWcs,
        trait_items: &[TraitItem],
        ii_fn: &Fn,
    ) -> Fallible<()> {
        let impl_assumptions: Wcs = impl_assumptions.to_wcs();
        assert!(
            env.only_universal_variables() && env.encloses((&impl_assumptions, trait_items, ii_fn))
        );

        // Find the corresponding function from the trait:
        let ti_fn = match trait_items
            .iter()
            .downcasted::<Fn>()
            .find(|trait_f| trait_f.id == ii_fn.id)
        {
            Some(trait_f) => trait_f,
            None => bail!("no fn `{:?}` in the trait", ii_fn.id),
        };

        tracing::debug!(?ti_fn);

        self.check_fn(env, &impl_assumptions, ii_fn)?;

        let mut env = env.clone();
        let (
            FnBoundData {
                input_tys: ii_input_tys,
                output_ty: ii_output_ty,
                where_clauses: ii_where_clauses,
                body: _,
            },
            FnBoundData {
                input_tys: ti_input_tys,
                output_ty: ti_output_ty,
                where_clauses: ti_where_clauses,
                body: _,
            },
        ) = env.instantiate_universally(&self.merge_binders(&ii_fn.binder, &ti_fn.binder)?);

        self.prove_goal(
            &env,
            (&impl_assumptions, &ti_where_clauses),
            &ii_where_clauses,
        )?;

        if ii_input_tys.len() != ti_input_tys.len() {
            bail!(
                "impl has {} function arguments but trait has {} function arguments",
                ii_input_tys.len(),
                ti_input_tys.len()
            )
        }

        for (ii_input_ty, ti_input_ty) in ii_input_tys.iter().zip(&ti_input_tys) {
            self.prove_goal(
                &env,
                (&impl_assumptions, &ii_where_clauses),
                Relation::sub(ti_input_ty, ii_input_ty),
            )?;
        }

        self.prove_goal(
            &env,
            (&impl_assumptions, &ii_where_clauses),
            Relation::sub(ii_output_ty, ti_output_ty),
        )?;

        Ok(())
    }

    #[context("check_associated_ty_value({impl_value:?})")]
    fn check_associated_ty_value(
        &self,
        impl_env: &Env,
        impl_assumptions: impl ToWcs,
        trait_items: &[TraitItem],
        impl_value: &AssociatedTyValue,
    ) -> Fallible<()> {
        let impl_assumptions: Wcs = impl_assumptions.to_wcs();

        assert!(
            impl_env.only_universal_variables()
                && impl_env.encloses((&impl_assumptions, trait_items, impl_value))
        );

        let AssociatedTyValue { id, binder } = impl_value;

        let trait_associated_ty = match trait_items
            .iter()
            .downcasted::<AssociatedTy>()
            .find(|trait_associated_ty| trait_associated_ty.id == *id)
        {
            Some(trait_associated_ty) => trait_associated_ty,
            None => bail!("no associated type `{:?}` in the trait", id),
        };

        let mut env = impl_env.clone();

        let (
            AssociatedTyValueBoundData {
                where_clauses: ii_where_clauses,
                ty: ii_ty,
            },
            AssociatedTyBoundData {
                ensures: ti_ensures,
                where_clauses: ti_where_clauses,
            },
        ) = env.instantiate_universally(&self.merge_binders(binder, &trait_associated_ty.binder)?);

        self.prove_where_clauses_well_formed(
            &env,
            (&impl_assumptions, &ii_where_clauses),
            &ii_where_clauses,
        )?;

        self.prove_goal(
            &env,
            (&impl_assumptions, &ti_where_clauses),
            &ii_where_clauses,
        )?;

        self.prove_goal(
            &env,
            (&impl_assumptions, &ii_where_clauses),
            ii_ty.well_formed(),
        )?;

        let ensures: Wcs = ti_ensures.iter().map(|e| e.to_wc(&ii_ty)).collect();
        self.prove_goal(&env, (&impl_assumptions, &ii_where_clauses), ensures)?;

        Ok(())
    }

    /// Given a binder from some impl item `I` and a binder from the corresponding trait item `T`,
    /// check that the binders have the same number/kinds of parameters, and then merge them
    /// into a single binder over `(I, T)`
    fn merge_binders<I: Term, T: Term>(
        &self,
        impl_binder: &Binder<I>,
        trait_binder: &Binder<T>,
    ) -> Fallible<Binder<(I, T)>> {
        if impl_binder.kinds() != trait_binder.kinds() {
            bail!(
                "distinct binder kinds: impl {:?} vs trait {:?}",
                impl_binder.kinds(),
                trait_binder.kinds()
            );
        }

        let (impl_names, impl_value) = impl_binder.open();

        let (trait_names, trait_value) = trait_binder.open();

        assert_eq!(impl_names.len(), trait_names.len());
        let trait_to_impl_subst: Substitution = trait_names.iter().zip(impl_names.iter()).collect();

        Ok(Binder::new(
            &impl_names,
            (impl_value, trait_to_impl_subst.apply(&trait_value)),
        ))
    }
}
