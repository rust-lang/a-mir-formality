use anyhow::bail;

use crate::grammar::{
    AssociatedTy, AssociatedTyBoundData, AssociatedTyValue, AssociatedTyValueBoundData, Binder,
    CrateId, Fallible, Fn, FnBoundData, ImplItem, MaybeFnBody, NegTraitImpl, NegTraitImplBoundData,
    Predicate, Relation, Substitution, Trait, TraitBoundData, TraitImpl, TraitImplBoundData,
    TraitItem, Wcs,
};
use crate::prove::prove::{Env, Program, Safety};
use crate::rust::Term;
use fn_error_context::context;
use formality_core::{judgment::ProofTree, judgment_fn, Downcasted};

judgment_fn! {
    pub(super) fn check_trait_impl(
        program: Program,
        trait_impl: TraitImpl,
        crate_id: CrateId,
    ) => () {
        debug(program, trait_impl, crate_id)
        (
            (let TraitImpl { binder, safety: _ } = &trait_impl)
            (let (env, bound_data) = Env::default().instantiate_universally(binder))
            (let TraitImplBoundData { trait_id, self_ty, trait_parameters, where_clauses, impl_items } = bound_data)
            (let trait_ref = trait_id.with(self_ty, trait_parameters))

            (super::where_clauses::prove_where_clauses_well_formed(program, env, where_clauses, &where_clauses) => ())
            (super::prove_goal(program, env, where_clauses, Predicate::is_implemented(trait_ref)) => ())
            (super::prove_not_goal(program, env, where_clauses, Predicate::not_implemented(trait_ref)) => ())

            (let trait_decl = program.program().trait_named(&trait_ref.trait_id)?)
            (let TraitBoundData { where_clauses: _, trait_items } = trait_decl.binder.instantiate_with(&trait_ref.parameters)?)
            (check_safety_matches(&trait_decl, &trait_impl) => ())

            (for_all(impl_item in impl_items)
                (check_trait_impl_item(program, env, where_clauses, &trait_items, impl_item, crate_id) => ()))

            (check_all_required_items_present(trait_items, impl_items) => ())

            ---- ("check_trait_impl")
            (check_trait_impl(program, trait_impl, crate_id) => ())
        )
    }
}

#[context("check_neg_trait_impl({trait_impl:?})")]
pub(super) fn check_neg_trait_impl(
    program: &Program,
    trait_impl: &NegTraitImpl,
) -> Fallible<ProofTree> {
    let NegTraitImpl { binder, safety } = trait_impl;

    let (
        env,
        NegTraitImplBoundData {
            trait_id,
            self_ty,
            trait_parameters,
            where_clauses,
        },
    ) = Env::default().instantiate_universally(binder);

    let trait_ref = trait_id.with(self_ty, trait_parameters);

    // Negative impls are always safe (rustc E0198) regardless of the trait's safety.
    if *safety == Safety::Unsafe {
        bail!("negative impls cannot be unsafe");
    }

    let mut proof_tree =
        ProofTree::new(format!("check_neg_trait_impl({trait_ref:?})"), None, vec![]);

    proof_tree
        .children
        .push(super::where_clauses::prove_where_clauses_well_formed(
            program,
            &env,
            &where_clauses,
            &where_clauses,
        )?);

    proof_tree.children.push(super::prove_goal(
        program,
        &env,
        &where_clauses,
        Predicate::not_implemented(&trait_ref),
    )?);

    Ok(proof_tree)
}

judgment_fn! {
    /// Validate that the declared safety of an impl matches the one from the trait declaration.
    fn check_safety_matches(
        trait_decl: Trait,
        trait_impl: TraitImpl,
    ) => () {
        debug(trait_decl, trait_impl)
        (
            (if trait_decl.safety == trait_impl.safety)
            ---- ("safety matches")
            (check_safety_matches(trait_decl, trait_impl) => ())
        )
    }
}

/// Check that every required trait item has a corresponding impl item.
/// A trait fn is required if it has no default body (`NoFnBody`).
/// Associated types are always required (no defaults supported yet).
fn check_all_required_items_present(
    trait_items: &[TraitItem],
    impl_items: &[ImplItem],
) -> Fallible<ProofTree> {
    for trait_item in trait_items {
        match trait_item {
            TraitItem::Fn(trait_fn) => {
                let (_, bound_data) = trait_fn.binder.open();
                if matches!(bound_data.body, MaybeFnBody::NoFnBody) {
                    if !impl_items
                        .iter()
                        .downcasted::<Fn>()
                        .any(|impl_fn| impl_fn.id == trait_fn.id)
                    {
                        bail!(
                            "not all trait items implemented, missing: `{:?}`",
                            trait_fn.id
                        );
                    }
                }
            }
            TraitItem::AssociatedTy(trait_assoc_ty) => {
                if !impl_items
                    .iter()
                    .downcasted::<AssociatedTyValue>()
                    .any(|impl_assoc| impl_assoc.id == trait_assoc_ty.id)
                {
                    bail!(
                        "not all trait items implemented, missing: `{:?}`",
                        trait_assoc_ty.id
                    );
                }
            }
        }
    }
    Ok(ProofTree::leaf("check_all_required_items_present"))
}

judgment_fn! {
    fn check_trait_impl_item(
        program: Program,
        env: Env,
        assumptions: Wcs,
        trait_items: Vec<TraitItem>,
        impl_item: ImplItem,
        crate_id: CrateId,
    ) => () {
        debug(program, env, assumptions, impl_item, crate_id)

        (
            (check_fn_in_impl(program, env, assumptions, trait_items, v, crate_id) => ())
            ---- ("fn in impl")
            (check_trait_impl_item(program, env, assumptions, trait_items, ImplItem::Fn(v), crate_id) => ())
        )

        (
            (check_associated_ty_value(program, env, assumptions, trait_items, v) => ())
            ---- ("associated ty value")
            (check_trait_impl_item(program, env, assumptions, trait_items, ImplItem::AssociatedTyValue(v), crate_id) => ())
        )
    }
}

judgment_fn! {
    fn check_fn_in_impl(
        program: Program,
        env: Env,
        impl_assumptions: Wcs,
        trait_items: Vec<TraitItem>,
        ii_fn: Fn,
        crate_id: CrateId,
    ) => () {
        debug(program, env, impl_assumptions, ii_fn, crate_id)
        (
            // Find the corresponding function from the trait
            (if let Some(ti_fn) = trait_items
                .iter()
                .downcasted::<Fn>()
                .find(|trait_f| trait_f.id == ii_fn.id))

            // Check the fn itself
            (super::fns::check_fn(program, env, impl_assumptions, ii_fn, crate_id) => ())

            // Merge binders and instantiate universally
            (let (env, (ii_bound, ti_bound)) = env.instantiate_universally(&merge_binders(&ii_fn.binder, &ti_fn.binder)?))
            (let FnBoundData { input_args: ii_input_args, output_ty: ii_output_ty, where_clauses: ii_where_clauses, body: _ } = ii_bound)
            (let FnBoundData { input_args: ti_input_args, output_ty: ti_output_ty, where_clauses: ti_where_clauses, body: _ } = ti_bound)

            // Prove impl where-clauses follow from trait where-clauses
            (super::prove_goal(program, &env, (&impl_assumptions, &ti_where_clauses), &ii_where_clauses) => ())

            // Check argument count matches
            (if ii_input_args.len() == ti_input_args.len())

            // Check each argument: trait arg is subtype of impl arg (contravariance)
            (for_all(pair in ii_input_args.iter().zip(ti_input_args.iter()))
                (let (ii_input_arg, ti_input_arg) = pair)
                (super::prove_goal(program, &env, (&impl_assumptions, &ii_where_clauses), Relation::sub(&ti_input_arg.ty, &ii_input_arg.ty)) => ()))

            // Check return type: impl return is subtype of trait return (covariance)
            (super::prove_goal(program, &env, (&impl_assumptions, &ii_where_clauses), Relation::sub(ii_output_ty, ti_output_ty)) => ())

            ---- ("check_fn_in_impl")
            (check_fn_in_impl(program, env, impl_assumptions, trait_items, ii_fn, crate_id) => ())
        )
    }
}

judgment_fn! {
    fn check_associated_ty_value(
        program: Program,
        impl_env: Env,
        impl_assumptions: Wcs,
        trait_items: Vec<TraitItem>,
        impl_value: AssociatedTyValue,
    ) => () {
        debug(program, impl_env, impl_assumptions, impl_value)
        (
            (let AssociatedTyValue { id, binder } = &impl_value)

            // Find the corresponding associated type from the trait
            (if let Some(trait_associated_ty) = trait_items
                .iter()
                .downcasted::<AssociatedTy>()
                .find(|trait_associated_ty| trait_associated_ty.id == *id))

            // Merge binders and instantiate universally
            (let (env, (ii_bound, ti_bound)) = impl_env.instantiate_universally(&merge_binders(binder, &trait_associated_ty.binder)?))
            (let AssociatedTyValueBoundData { where_clauses: ii_where_clauses, ty: ii_ty } = ii_bound)
            (let AssociatedTyBoundData { ensures: ti_ensures, where_clauses: ti_where_clauses } = ti_bound)

            // Prove impl where-clauses are well-formed
            (super::where_clauses::prove_where_clauses_well_formed(program, &env, (&impl_assumptions, &ii_where_clauses), &ii_where_clauses) => ())

            // Prove impl where-clauses follow from trait where-clauses
            (super::prove_goal(program, &env, (&impl_assumptions, &ti_where_clauses), &ii_where_clauses) => ())

            // Prove the impl type is well-formed
            (super::prove_goal(program, &env, (&impl_assumptions, &ii_where_clauses), Relation::well_formed(ii_ty)) => ())

            // Prove the ensures clauses
            (let ensures: Wcs = ti_ensures.iter().map(|e| e.to_wc(&ii_ty)).collect())
            (super::prove_goal(program, &env, (&impl_assumptions, &ii_where_clauses), ensures) => ())

            ---- ("check_associated_ty_value")
            (check_associated_ty_value(program, impl_env, impl_assumptions, trait_items, impl_value) => ())
        )
    }
}

/// Given a binder from some impl item `I` and a binder from the corresponding trait item `T`,
/// check that the binders have the same number/kinds of parameters, and then merge them
/// into a single binder over `(I, T)`
fn merge_binders<I: Term, T: Term>(
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
