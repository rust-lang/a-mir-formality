use formality_core::{judgment_fn, Set};
use formality_types::grammar::{
    AliasName, AliasTy, ConstData, Parameter, Parameters, RigidName, RigidTy, UniversalVar, Wcs,
};

use crate::{decls::Decls, prove::combinators::for_all};

use super::{constraints::Constraints, env::Env};

judgment_fn! {
    pub fn prove_wf(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        goal: Parameter,
    ) => Constraints {
        debug(goal, assumptions, env, decls)

        assert(env.encloses((&assumptions, &goal)))

        (
            // Always assume that universal variables are WF. This is debatable, it implies
            // that we ensure by construction that the values we infer for existential variables
            // are WF. An alternative would be to add explicit assumptions into the environment
            // for every universal variable. That just seems tedious.
            --- ("universal variables")
            (prove_wf(_decls, env, _assumptions, UniversalVar { .. }) => Constraints::none(env))
        )

        (
            (for_all(&decls, &env, &assumptions, &parameters, &prove_wf) => c)
            --- ("tuples")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::Tuple(_), parameters }) => c)
        )

        (
            (for_all(&decls, &env, &assumptions, &parameters, &prove_wf) => c)
            --- ("integers and booleans")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::ScalarId(_), parameters }) => c)
        )

        (
            (for_all(&decls, &env, &assumptions, &parameters, &prove_wf) => c)
            --- ("ADT")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::AdtId(_), parameters }) => c)
        )

        (
            (prove_wf(&decls, &env, &assumptions, ty) => c)
            --- ("rigid constants")
            (prove_wf(decls, env, assumptions, ConstData::Value(_, ty)) => c)
        )

        (
            (prove_alias_wf(&decls, &env, &assumptions, name, parameters) => c)
            --- ("aliases")
            (prove_wf(decls, env, assumptions, AliasTy { name, parameters }) => c)
        )
    }
}

pub fn prove_alias_wf(
    decls: &Decls,
    env: &Env,
    assumptions: &Wcs,
    _name: AliasName,
    parameters: Parameters,
) -> Set<Constraints> {
    // FIXME: verify self type implements trait
    for_all(decls, env, assumptions, &parameters, &prove_wf)
}
