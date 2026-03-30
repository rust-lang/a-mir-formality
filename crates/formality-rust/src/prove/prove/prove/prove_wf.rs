use crate::grammar::{
    AliasName, AliasTy, ConstData, Lt, LtData, Parameter, Parameters, Relation, RigidName, RigidTy,
    Ty, UniversalVar, Wcs,
};
use formality_core::{judgment_fn, Downcast, ProvenSet};

use crate::prove::prove::{
    decls::Program,
    prove::{combinators::for_all, prove_after::prove_after, prove_wc::prove_wc},
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
            (prove_wc(decls, env, assumptions, Relation::outlives(ty, lt)) => c)
            --- ("references")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::Ref(_), parameters }) => c)
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
