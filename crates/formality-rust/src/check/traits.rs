use crate::grammar::{
    AliasTy, AssociatedTy, AssociatedTyBoundData, Fn, Predicate, Trait, TraitBoundData, TraitItem,
    TraitRef, Ty, Wcs, WhereBound,
};
use crate::grammar::{CrateId, Fallible};
use crate::prove::{Env, Program};
use anyhow::bail;
use formality_core::{judgment::ProofTree, judgment_fn, seq, Set, Upcasted};

judgment_fn! {
    pub(super) fn check_trait(
        program: Program,
        env: Env,
        t: Trait,
        crate_id: CrateId,
    ) => () {
        debug(program, t, crate_id)

        (
            (let Trait { safety: _, id, binder } = t)
            (let (env, parameters) = env.universal_substitution(&binder.explicit_binder))
            (let bound_data = binder.instantiate_with(parameters)?)
            (let trait_ref = TraitRef::new(id, parameters))
            (let TraitBoundData { where_clauses, trait_items } = bound_data)
            (check_trait_items_have_unique_names(&trait_items) => ())
            (super::where_clauses::prove_where_clauses_well_formed(
                program, env, (where_clauses, Predicate::is_implemented(&trait_ref)), where_clauses,
            ) => ())
            (for_all(trait_item in trait_items)
                (check_trait_item(program, env, trait_ref, where_clauses, trait_item, crate_id) => ()))
            ------------------------------------------------------------ ("check trait")
            (check_trait(program, env, t, crate_id) => ())
        )
    }
}

judgment_fn! {
    fn check_trait_item(
        program: Program,
        env: Env,
        trait_ref: TraitRef,
        where_clauses: Wcs,
        trait_item: TraitItem,
        crate_id: CrateId,
    ) => () {
        debug(program, env, where_clauses, trait_item, crate_id)

        (
            (check_fn_in_trait(program, env, (where_clauses, trait_ref), f, crate_id) => ())
            ------------------------------------------------------------ ("fn in trait")
            (check_trait_item(program, env, trait_ref, where_clauses, TraitItem::Fn(f), crate_id) => ())
        )

        (
            (check_associated_ty(program, env, trait_ref, where_clauses, v) => ())
            ------------------------------------------------------------ ("associated ty in trait")
            (check_trait_item(program, env, trait_ref, where_clauses, TraitItem::AssociatedTy(v), crate_id) => ())
        )
    }
}

judgment_fn! {
    fn check_fn_in_trait(
        program: Program,
        env: Env,
        assumptions: Wcs,
        f: Fn,
        crate_id: CrateId,
    ) => () {
        debug(program, env, assumptions, f, crate_id)

        (
            (super::fns::check_fn(program, env, assumptions, f, crate_id) => ())
            ------------------------------------------------------------ ("check fn in trait")
            (check_fn_in_trait(program, env, assumptions, f, crate_id) => ())
        )
    }
}

judgment_fn! {
    fn check_associated_ty(
        program: Program,
        env: Env,
        trait_ref: TraitRef,
        trait_where_clauses: Wcs,
        associated_ty: AssociatedTy,
    ) => () {
        debug(program, env, trait_where_clauses, associated_ty)

        (
            (let AssociatedTy { id, binder } = associated_ty)
            (let (env, parameters) = env.universal_substitution(binder))
            (let AssociatedTyBoundData { ensures, where_clauses } = binder.instantiate_with(parameters)?)
            (let self_ty = AliasTy::associated_ty(
                &trait_ref.trait_id, id, parameters.len(),
                seq![..trait_ref.parameters.iter().cloned(), ..parameters.iter().upcasted()],
            ))

            (super::where_clauses::prove_where_clauses_well_formed(
                program, env, (trait_where_clauses, where_clauses, Predicate::is_implemented(trait_ref)), where_clauses,
            ) => ())
            (for_all(bound in ensures)
                (check_where_bound(program, env, (trait_where_clauses, where_clauses, trait_ref), self_ty, bound) => ()))

            ------------------------------------------------------------ ("check associated ty")
            (check_associated_ty(program, env, trait_ref, trait_where_clauses, associated_ty) => ())
        )
    }
}

judgment_fn! {
    fn check_where_bound(
        program: Program,
        env: Env,
        assumptions: Wcs,
        self_ty: Ty,
        bound: WhereBound,
    ) => () {
        debug(env, assumptions, self_ty, bound)

        (
            (let trait_ref = trait_id.with(self_ty, parameters))
            (let trait_decl = program.program().trait_named(trait_id)?)
            (let _data = trait_decl.binder.instantiate_with(&trait_ref.parameters)?)
            (super::prove_goal(program, env, assumptions, Predicate::well_formed_trait_ref(trait_ref)) => ())
            ------------------------------------------------------------ ("trait bound")
            (check_where_bound(program, env, assumptions, self_ty, WhereBound::IsImplemented(trait_id, parameters)) => ())
        )

        (
            (let trait_ref = trait_id.with(self_ty, parameters))
            (let alias = AliasTy::associated_ty(
                trait_id, item_id, item_parameters.len(),
                seq![..trait_ref.parameters.iter().cloned(), ..item_parameters.iter().cloned()],
            ))
            (super::prove_goal(program, env, assumptions, (
                Predicate::well_formed(alias), Predicate::well_formed(ty),
            )) => ())
            ------------------------------------------------------------ ("associated equality bound")
            (check_where_bound(program, env, assumptions, self_ty, WhereBound::AliasEq(trait_id, parameters, item_id, item_parameters, ty)) => ())
        )

        (
            (super::prove_goal(program, env, assumptions, (Predicate::well_formed(self_ty), Predicate::well_formed(lt))) => ())
            ------------------------------------------------------------ ("outlives bound")
            (check_where_bound(program, env, assumptions, self_ty, WhereBound::Outlives(lt)) => ())
        )

        (
            (let (env, bound) = env.instantiate_universally(binder))
            (check_where_bound(program, env, assumptions, self_ty, bound) => ())
            ------------------------------------------------------------ ("quantified bound")
            (check_where_bound(program, env, assumptions, self_ty, WhereBound::ForAll(binder)) => ())
        )
    }
}

fn check_trait_items_have_unique_names(trait_items: &[TraitItem]) -> Fallible<ProofTree> {
    let mut functions = Set::new();
    let mut associated_types = Set::new();
    for trait_item in trait_items {
        match trait_item {
            TraitItem::Fn(f) => {
                if !functions.insert(&f.id) {
                    bail!("the function name `{:?}` is defined multiple times", f.id);
                }
            }
            TraitItem::AssociatedTy(AssociatedTy { id, .. }) => {
                if !associated_types.insert(id) {
                    bail!(
                        "the associated type name `{:?}` is defined multiple times",
                        id
                    );
                }
            }
        }
    }
    Ok(ProofTree::leaf("check_trait_items_have_unique_names"))
}
