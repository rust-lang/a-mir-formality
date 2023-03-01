use anyhow::bail;
use contracts::requires;
use fn_error_context::context;
use formality_decl::grammar::{
    AssociatedTy, AssociatedTyBoundData, AssociatedTyValue, AssociatedTyValueBoundData, Fn,
    FnBoundData, ImplItem, TraitBoundData, TraitImpl, TraitImplBoundData, TraitItem,
};
use formality_logic::Env;
use formality_types::{
    cast::{Downcasted, To, Upcasted},
    grammar::{Binder, Fallible, Goal, Hypothesis, Substitution},
    term::Term,
    visit::Visit,
};

impl super::Check<'_> {
    #[context("check_trait_impl({v:?})")]
    pub(super) fn check_trait_impl(&self, v: &TraitImpl) -> Fallible<()> {
        let TraitImpl { binder } = v;

        let mut env = Env::default();

        let TraitImplBoundData {
            trait_ref,
            where_clauses,
            impl_items,
        } = env.instantiate_universally(binder);

        let assumptions: Vec<Hypothesis> = where_clauses.to();

        self.prove_where_clauses_well_formed(&env, &assumptions, &where_clauses)?;

        self.prove_goal(&env, &assumptions, trait_ref.is_implemented())?;

        let trait_decl = self.program.trait_named(&trait_ref.trait_id)?;
        let TraitBoundData {
            where_clauses: _,
            trait_items,
        } = trait_decl.binder.instantiate_with(&trait_ref.parameters)?;

        for impl_item in &impl_items {
            self.check_trait_impl_item(&env, &assumptions, &trait_items, impl_item)?;
        }

        Ok(())
    }

    #[requires(assumptions.iter().all(|a| a.references_only_placeholder_variables()))]
    fn check_trait_impl_item(
        &self,
        env: &Env,
        assumptions: &[Hypothesis],
        trait_items: &[TraitItem],
        impl_item: &ImplItem,
    ) -> Fallible<()> {
        match impl_item {
            ImplItem::Fn(v) => self.check_fn_in_impl(env, assumptions, trait_items, v),
            ImplItem::AssociatedTyValue(v) => {
                self.check_associated_ty_value(env, assumptions, trait_items, v)
            }
        }
    }

    #[context("check_fn_in_impl")]
    #[requires(impl_assumptions.iter().all(|a| a.references_only_placeholder_variables()))]
    fn check_fn_in_impl(
        &self,
        env: &Env,
        impl_assumptions: &[Hypothesis],
        trait_items: &[TraitItem],
        ii_fn: &Fn,
    ) -> Fallible<()> {
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

        self.check_fn(env, impl_assumptions, ii_fn)?;

        let mut env = env.clone();
        let (
            FnBoundData {
                input_tys: ii_input_tys,
                output_ty: ii_output_ty,
                where_clauses: ii_where_clauses,
            },
            FnBoundData {
                input_tys: ti_input_tys,
                output_ty: ti_output_ty,
                where_clauses: ti_where_clauses,
            },
        ) = env.instantiate_universally(&self.merge_binders(&ii_fn.binder, &ti_fn.binder)?);

        self.prove_goal(
            &env,
            (impl_assumptions, &ti_where_clauses),
            Goal::all(&ii_where_clauses),
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
                (impl_assumptions, &ii_where_clauses),
                Goal::sub(ti_input_ty, ii_input_ty),
            )?;
        }

        self.prove_goal(
            &env,
            (impl_assumptions, &ii_where_clauses),
            Goal::sub(ii_output_ty, ti_output_ty),
        )?;

        Ok(())
    }

    fn check_associated_ty_value(
        &self,
        impl_env: &Env,
        impl_assumptions: &[Hypothesis],
        trait_items: &[TraitItem],
        impl_value: &AssociatedTyValue,
    ) -> Fallible<()> {
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
            (impl_assumptions, &ii_where_clauses),
            &ii_where_clauses,
        )?;

        self.prove_goal(
            &env,
            (impl_assumptions, &ti_where_clauses),
            Goal::all(&ii_where_clauses),
        )?;

        self.prove_goal(
            &env,
            (impl_assumptions, &ii_where_clauses),
            ii_ty.well_formed(),
        )?;

        let ensures = ti_ensures.instantiate_with(&[ii_ty])?;
        self.prove_goal(
            &env,
            (impl_assumptions, &ii_where_clauses),
            Goal::all(ensures),
        )?;

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
        let trait_to_impl_subst: Substitution = trait_names
            .iter()
            .upcasted()
            .zip(impl_names.iter().upcasted())
            .collect();

        Ok(Binder::new(
            &impl_names,
            (impl_value, trait_to_impl_subst.apply(&trait_value)),
        ))
    }
}
