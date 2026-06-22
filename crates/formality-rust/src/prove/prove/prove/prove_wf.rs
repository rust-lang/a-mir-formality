use crate::grammar::{
    AliasName, AliasTy, ConstData, Lt, LtData, Parameter, Parameters, Relation, RigidName, RigidTy,
    Ty, UniversalVar, Wcs,
};
use formality_core::{judgment_fn, Downcast, ProvenSet, Upcast};

use crate::prove::prove::{
    decls::Program,
    prove::{combinators::for_all, prove},
};

use super::env::Env;

judgment_fn! {
    pub fn prove_wf(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        goal: Parameter,
    ) => Env {
        debug(goal, assumptions, env)

        assert(env.encloses((assumptions, goal)))

        (
            // Always assume that universal variables are WF. This is debatable, it implies
            // that we ensure by construction that the values we infer for existential variables
            // are WF. An alternative would be to add explicit assumptions into the environment
            // for every universal variable. That just seems tedious.
            --- ("universal variables")
            (prove_wf(_decls, env, _assumptions, UniversalVar { .. }) => env)
        )

        (
            // `&'a T` is well-formed if `T: 'a`
            (let (lt, ty) = parameters.downcast_err::<(Lt, Ty)>()?)
            (prove_wf_recursive(decls, env, assumptions, ty) => env)
            (prove(decls, env, assumptions, Relation::outlives(ty, lt)) => env)
            --- ("references")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::Ref(_), parameters }) => env)
        )

        (
            // `*const T`/`*mut T` is well-formed if `T` is.
            (let (ty,) = parameters.downcast_err::<(Ty,)>()?)
            (prove_wf_recursive(decls, env, assumptions, ty) => env)
            --- ("raw-pointers")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::Raw(_), parameters }) => env)
        )

        (
            (for_all(decls, env, assumptions, parameters, &prove_wf_recursive) => env)
            --- ("tuples")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::Tuple(_), parameters }) => env)
        )

        (
            (for_all(decls, env, assumptions, parameters, &prove_wf_recursive) => env)
            --- ("integers and booleans")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::ScalarId(_), parameters }) => env)
        )

        (
            (for_all(decls, env, assumptions, parameters, &prove_wf_recursive) => env)
            (let t = decls.program().adt_item_named(adt_id)?.to_adt())
            (let t = t.binder.instantiate_with(parameters).unwrap())
            (prove(decls, env, assumptions, &t.where_clauses) => env)
            --- ("ADT")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::AdtId(adt_id), parameters }) => env)
        )

        (
            --- ("static lifetime")
            (prove_wf(_decls, env, _assumptions, LtData::Static) => env)
        )

        (
            --- ("scalar constants are always wf")
            (prove_wf(_decls, env, _assumptions, ConstData::Scalar(_)) => env)
        )

        (
            (prove_alias_wf(decls, env, assumptions, name, parameters) => env)
            --- ("aliases")
            (prove_wf(decls, env, assumptions, AliasTy { name, parameters }) => env)
        )
    }
}

pub fn prove_alias_wf(
    decls: &Program,
    env: &Env,
    assumptions: &Wcs,
    _name: &AliasName,
    parameters: &Parameters,
) -> ProvenSet<Env> {
    // FIXME(#217): verify self type implements trait
    for_all(decls, env, assumptions, parameters, &prove_wf_recursive)
}

pub fn prove_wf_recursive(
    program: impl Upcast<Program>,
    env: impl Upcast<Env>,
    assumptions: impl Upcast<Wcs>,
    param: impl Upcast<Parameter>,
) -> ProvenSet<Env> {
    prove(program, env, assumptions, Relation::well_formed(param))
}
