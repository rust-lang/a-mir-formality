use crate::grammar::{
    AliasName, AliasTy, AssociatedTy, AssociatedTyName, Const, Lt, Parameter, Parameters,
    Predicate, RigidName, RigidTy, TraitRef, Ty, UniversalVar, Wcs,
};
use formality_core::{judgment_fn, Downcast, Downcasted, ProvenSet, Upcast};

use crate::prove::{combinators::for_all, decls::Program, prove, prove_after::prove_after};

use super::{constraints::Constraints, env::Env};

judgment_fn! {
    pub fn prove_wf(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        goal: Parameter,
    ) => Constraints {
        debug(goal, assumptions, env)

        assert(env.encloses((assumptions, goal)))

        (
            // Always assume that universal variables are WF. This is debatable, it implies
            // that we ensure by construction that the values we infer for existential variables
            // are WF. An alternative would be to add explicit assumptions into the environment
            // for every universal variable. That just seems tedious.
            --- ("universal variables")
            (prove_wf(_decls, env, _assumptions, UniversalVar { .. }) => Constraints::none(env))
        )

        (
            // `&'a T` is well-formed if `T: 'a`
            (let (lt, ty) = parameters.downcast_err::<(Lt, Ty)>()?)
            (prove_wf_recursive(decls, env, assumptions, ty) => c)
            (prove_after(decls, c, assumptions, Predicate::outlives(ty, lt)) => c)
            --- ("references")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::Ref(_), parameters }) => c)
        )

        (
            // `*const T`/`*mut T` is well-formed if `T` is.
            (let (ty,) = parameters.downcast_err::<(Ty,)>()?)
            (prove_wf_recursive(decls, env, assumptions, ty) => c)
            --- ("raw-pointers")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::Raw(_), parameters }) => c)
        )

        (
            (for_all(decls, env, assumptions, parameters, &prove_wf_recursive) => c)
            --- ("tuples")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::Tuple(_), parameters }) => c)
        )

        (
            (for_all(decls, env, assumptions, parameters, &prove_wf_recursive) => c)
            --- ("integers and booleans")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::ScalarId(_), parameters }) => c)
        )

        (
            (for_all(decls, env, assumptions, parameters, &prove_wf_recursive) => c)
            (let t = decls.program().adt_item_named(adt_id)?.to_adt())
            (let t = t.binder.instantiate_with(parameters).unwrap())
            (prove_after(decls, c, assumptions, &t.where_clauses) => c)
            --- ("ADT")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::AdtId(adt_id), parameters }) => c)
        )

        (
            --- ("static lifetime")
            (prove_wf(_decls, env, _assumptions, Lt::Static) => Constraints::none(env))
        )

        (
            --- ("scalar constants are always wf")
            (prove_wf(_decls, env, _assumptions, Const::Scalar(_)) => Constraints::none(env))
        )

        (
            (prove_alias_wf(decls, env, assumptions, name, parameters) => c)
            --- ("aliases")
            (prove_wf(decls, env, assumptions, AliasTy { name, parameters }) => c)
        )
    }
}

judgment_fn! {
    pub fn prove_alias_wf(
        decls: Program,
        env: Env,
        assumptions: Wcs,
        name: AliasName,
        parameters: Parameters,
    ) => Constraints {
        debug(name, parameters, assumptions, env)

        (
            (let trait_arity = parameters.len().checked_sub(*item_arity))
            (if let Some(trait_arity) = trait_arity)
            (let (trait_parameters, item_parameters) = parameters.split_at(*trait_arity))
            (let trait_decl = decls.program().trait_named(trait_id)?)
            (let trait_data = trait_decl.binder.instantiate_with(trait_parameters)?)
            (if let Some(item) = trait_data.trait_items.iter().downcasted::<AssociatedTy>().find(|item| item.id == *item_id))
            (let item_data = item.binder.instantiate_with(item_parameters)?)
            (for_all(decls, env, assumptions, parameters, &prove_wf_recursive) => c)
            (let trait_ref = TraitRef::new(trait_id, trait_parameters))
            (prove_after(decls, c, assumptions, (
                trait_ref, (&trait_data.where_clauses, &item_data.where_clauses),
            )) => c)
            --- ("associated type")
            (prove_alias_wf(decls, env, assumptions, AssociatedTyName { trait_id, item_id, item_arity }, parameters) => c)
        )
    }
}

pub fn prove_wf_recursive(
    program: impl Upcast<Program>,
    env: impl Upcast<Env>,
    assumptions: impl Upcast<Wcs>,
    param: impl Upcast<Parameter>,
) -> ProvenSet<Constraints> {
    prove(program, env, assumptions, Predicate::well_formed(param))
}
