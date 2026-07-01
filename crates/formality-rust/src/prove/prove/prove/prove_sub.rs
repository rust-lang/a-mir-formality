use crate::grammar::{Lt, Parameter, Relation, RigidTy, TyData, Wcs};
use crate::prove::prove::Constrained;
use formality_core::judgment_fn;

use crate::prove::prove::prove::prove_outlives::prove_outlives;
use crate::prove::prove::{
    decls::Program,
    prove::{prove, prove_normalize::prove_normalize},
};

use super::env::Env;

judgment_fn! {
    pub fn prove_sub(
        _decls: Program,
        env: Env,
        assumptions: Wcs,
        a: Parameter,
        b: Parameter,
    ) => Env {
        debug(a, b, assumptions, env)

        assert(a.kind() == b.kind())

        trivial(a == b => env)

        (
            (prove_normalize(decls, env, assumptions, x) => Constrained(y, env))
            (prove(decls, env, assumptions, Relation::sub(y, z)) => env)
            ----------------------------- ("normalize-l")
            (prove_sub(decls, env, assumptions, x, z) => env)
        )

        (
            (prove_normalize(decls, env, assumptions, y) => Constrained(z, env))
            (prove(decls, env, assumptions, Relation::sub(x, &z)) => env)
            ----------------------------- ("normalize-r")
            (prove_sub(decls, env, assumptions, x, y) => env)
        )

        (
            (let RigidTy { name: a_name, parameters: a_parameters } = a)
            (let RigidTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)!
            (prove(decls, env, assumptions, Wcs::all_sub(a_parameters, b_parameters)) => env)
            ----------------------------- ("rigid")
            (prove_sub(decls, env, assumptions, TyData::RigidTy(a), TyData::RigidTy(b)) => env)
        )

        (
            (prove_outlives(decls, env, assumptions, a, b) => env)
            ----------------------------- ("lifetime => outlives")
            (prove_sub(decls, env, assumptions, a: Lt, b: Lt) => env)
        )
    }
}
