use anyhow::bail;

use crate::prove::prove::{Env, Safety};
use crate::{
    grammar::{
        AssociatedTy, AssociatedTyBoundData, AssociatedTyValue, AssociatedTyValueBoundData, Fn,
        FnBoundData, ImplItem, NegTraitImpl, NegTraitImplBoundData, Trait, TraitBoundData,
        TraitImpl, TraitImplBoundData, TraitItem,
    },
    prove::ToWcs,
};
use fn_error_context::context;
use formality_core::{judgment::ProofTree, Downcasted};
use crate::types::{
    grammar::{Binder, CrateId, Fallible, Relation, Substitution, Wcs},
    rust::Term,
};

impl super::Check<'_> {
    #[context("check_trait_impl({trait_impl:?})")]
    pub(super) fn check_trait_impl(
        &self,
        trait_impl: &TraitImpl,
        crate_id: &CrateId,
    ) -> Fallible<ProofTree> {
        let TraitImpl { binder, safety: _ } = trait_impl;
        let mut proof_tree = ProofTree::leaf("check_trait_impl");

        let mut env = Env::default();

        let TraitImplBoundData {
            trait_id,
            self_ty,
            trait_parameters,
            where_clauses,
            impl_items,
        } = env.instantiate_universally(binder);

        let trait_ref = trait_id.with(self_ty, trait_parameters);

        proof_tree
            .children
            .push(self.prove_where_clauses_well_formed(&env, &where_clauses, &where_clauses)?);

        proof_tree.children.push(self.prove_goal(
            &env,
            &where_clauses,
            trait_ref.is_implemented(),
        )?);

        proof_tree.children.push(self.prove_not_goal(
            &env,
            &where_clauses,
            trait_ref.not_implemented(),
        )?);

        let trait_decl = self.program.trait_named(&trait_ref.trait_id)?;
        let TraitBoundData {
            where_clauses: _,
            trait_items,
        } = trait_decl.binder.instantiate_with(&trait_ref.parameters)?;

        proof_tree
            .children
            .push(self.check_safety_matches(trait_decl, trait_impl)?);

        for impl_item in &impl_items {
            proof_tree.children.push(self.check_trait_impl_item(
                &env,
                &where_clauses,
                &trait_items,
                impl_item,
                crate_id,
            )?);
        }

        Ok(proof_tree)
    }

    #[context("check_neg_trait_impl({trait_impl:?})")]
    pub(super) fn check_neg_trait_impl(&self, trait_impl: &NegTraitImpl) -> Fallible<ProofTree> {
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

        let mut proof_tree =
            ProofTree::new(format!("check_neg_trait_impl({trait_ref:?})"), None, vec![]);

        proof_tree
            .children
            .push(self.prove_where_clauses_well_formed(&env, &where_clauses, &where_clauses)?);

        proof_tree.children.push(self.prove_goal(
            &env,
            &where_clauses,
            trait_ref.not_implemented(),
        )?);

        Ok(proof_tree)
    }

    /// Validate that the declared safety of an impl matches the one from the trait declaration.
    fn check_safety_matches(
        &self,
        trait_decl: &Trait,
        trait_impl: &TraitImpl,
    ) -> Fallible<ProofTree> {
        let proof_tree = ProofTree::leaf(format!(
            "safety_matches({:?}, {:?})",
            trait_decl.safety, trait_impl.safety
        ));

        if trait_decl.safety != trait_impl.safety {
            match trait_decl.safety {
                Safety::Safe => bail!("implementing the trait `{:?}` is not unsafe", trait_decl.id),
                Safety::Unsafe => bail!(
                    "the trait `{:?}` requires an `unsafe impl` declaration",
                    trait_decl.id
                ),
            }
        }

        Ok(proof_tree)
    }

    fn check_trait_impl_item(
        &self,
        env: &Env,
        assumptions: impl ToWcs,
        trait_items: &[TraitItem],
        impl_item: &ImplItem,
        crate_id: &CrateId,
    ) -> Fallible<ProofTree> {
        let assumptions: Wcs = assumptions.to_wcs();
        assert!(
            env.only_universal_variables() && env.encloses((&assumptions, trait_items, impl_item))
        );

        match impl_item {
            ImplItem::Fn(v) => self.check_fn_in_impl(env, &assumptions, trait_items, v, crate_id),
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
        crate_id: &CrateId,
    ) -> Fallible<ProofTree> {
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

        let mut proof_tree =
            ProofTree::new(format!("check_fn_in_impl({:?})", ii_fn.id), None, vec![]);

        proof_tree
            .children
            .push(self.check_fn(env, &impl_assumptions, ii_fn, crate_id)?);

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

        proof_tree.children.push(self.prove_goal(
            &env,
            (&impl_assumptions, &ti_where_clauses),
            &ii_where_clauses,
        )?);

        if ii_input_tys.len() != ti_input_tys.len() {
            bail!(
                "impl has {} function arguments but trait has {} function arguments",
                ii_input_tys.len(),
                ti_input_tys.len()
            )
        }

        for (ii_input_ty, ti_input_ty) in ii_input_tys.iter().zip(&ti_input_tys) {
            proof_tree.children.push(self.prove_goal(
                &env,
                (&impl_assumptions, &ii_where_clauses),
                Relation::sub(ti_input_ty, ii_input_ty),
            )?);
        }

        // Check that the impl's declared return type is a subtype of what the trait declared:
        //
        // OK
        //
        // ```rust
        // trait Foo {
        //     fn bar<'a>(&'a self, input: &u32) -> &'a u32;
        // }
        //
        // impl Foo for MyType {
        //     fn bar(&self, input: &u32) -> &'static u32 {} // <-- subtype, ok
        // }
        // ```
        //
        // NOT OK
        //
        // ```rust
        // trait Foo {
        //     fn bar<'a>(&'a self, input: &u32) -> &'a u32;
        // }
        //
        // impl Foo for MyType {
        //     fn bar<'b>(&self, input: &'b u32) -> &'b u32 {} // <-- not sutype, not ok
        // }
        // ```

        proof_tree.children.push(self.prove_goal(
            &env,
            (&impl_assumptions, &ii_where_clauses),
            Relation::sub(ii_output_ty, ti_output_ty),
        )?);

        Ok(proof_tree)
    }

    #[context("check_associated_ty_value({impl_value:?})")]
    fn check_associated_ty_value(
        &self,
        impl_env: &Env,
        impl_assumptions: impl ToWcs,
        trait_items: &[TraitItem],
        impl_value: &AssociatedTyValue,
    ) -> Fallible<ProofTree> {
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

        let mut proof_tree = ProofTree::new(
            format!("check_associated_ty_value({:?})", impl_value.id),
            None,
            vec![],
        );

        proof_tree
            .children
            .push(self.prove_where_clauses_well_formed(
                &env,
                (&impl_assumptions, &ii_where_clauses),
                &ii_where_clauses,
            )?);

        proof_tree.children.push(self.prove_goal(
            &env,
            (&impl_assumptions, &ti_where_clauses),
            &ii_where_clauses,
        )?);

        proof_tree.children.push(self.prove_goal(
            &env,
            (&impl_assumptions, &ii_where_clauses),
            ii_ty.well_formed(),
        )?);

        let ensures: Wcs = ti_ensures.iter().map(|e| e.to_wc(&ii_ty)).collect();
        proof_tree.children.push(self.prove_goal(
            &env,
            (&impl_assumptions, &ii_where_clauses),
            ensures,
        )?);

        Ok(proof_tree)
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
