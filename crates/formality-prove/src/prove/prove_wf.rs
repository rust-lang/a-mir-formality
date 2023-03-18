use formality_types::{
    grammar::{Parameter, PlaceholderVar, RigidName, RigidTy, Wcs},
    judgment_fn,
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
            (prove_wf(_decls, env, _assumptions, PlaceholderVar { .. }) => Constraints::none(env))
        )
 
        (
            (for_all(&decls, &env, &assumptions, &parameters, &prove_wf) => c)
            --- ("tuples")
            (prove_wf(decls, env, assumptions, RigidTy { name: RigidName::Tuple(_), parameters }) => c)
        )
    }
}
