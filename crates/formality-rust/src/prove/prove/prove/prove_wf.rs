use crate::{
    grammar::{
        AliasName, AliasTy, ConstData, Lt, LtData, Parameter, Parameters, Relation, RigidName,
        RigidTy, Ty, UniversalVar, Wcs,
    },
    prove::prove::prove::prove_via::prove_via,
};
use formality_core::{judgment_fn, Downcast, ProvenSet};

use crate::prove::prove::{
    decls::Program,
    prove::{combinators::for_all, prove_after::prove_after},
};

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
            (a in assumptions)!
            (prove_via(decls, env, assumptions, a, Relation::WellFormed(goal.clone())) => c)
            --- ("by-assumption")
            (prove_wf(decls, env, assumptions, goal) => c)
        )

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
            (prove_wf(decls, env, assumptions, ty) => c)
            (prove_after(decls, c, assumptions, Relation::outlives(ty, lt)) => c)
            --- ("references")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::Ref(_), parameters }) => c)
        )

        (
            // `*const T`/`*mut T` is well-formed if `T` is.
            (let (ty,) = parameters.downcast_err::<(Ty,)>()?)
            (prove_wf(decls, env, assumptions, ty) => c)
            --- ("raw-pointers")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::Raw(_), parameters }) => c)
        )

        (
            (for_all(decls, env, assumptions, parameters, &prove_wf) => c)
            --- ("tuples")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::Tuple(_), parameters }) => c)
        )

        (
            (for_all(decls, env, assumptions, parameters, &prove_wf) => c)
            --- ("integers and booleans")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::ScalarId(_), parameters }) => c)
        )

        (
            (for_all(decls, env, assumptions, parameters, &prove_wf) => c)
            (let t = decls.program().adt_item_named(adt_id)?.to_adt())
            (let t = t.binder.instantiate_with(parameters).unwrap())
            (prove_after(decls, c, assumptions, &t.where_clauses) => c)
            --- ("ADT")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::AdtId(adt_id), parameters }) => c)
        )

        (
            --- ("static lifetime")
            (prove_wf(_decls, env, _assumptions, LtData::Static) => Constraints::none(env))
        )

        (
            --- ("scalar constants are always wf")
            (prove_wf(_decls, env, _assumptions, ConstData::Scalar(_)) => Constraints::none(env))
        )

        (
            (prove_alias_wf(decls, env, assumptions, name, parameters) => c)
            --- ("aliases")
            (prove_wf(decls, env, assumptions, AliasTy { name, parameters }) => c)
        )
    }
}

pub fn prove_alias_wf(
    decls: &Program,
    env: &Env,
    assumptions: &Wcs,
    _name: &AliasName,
    parameters: &Parameters,
) -> ProvenSet<Constraints> {
    // FIXME(#217): verify self type implements trait
    for_all(decls, env, assumptions, parameters, &prove_wf)
}
