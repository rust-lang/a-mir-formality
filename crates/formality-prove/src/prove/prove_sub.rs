use formality_core::judgment_fn;
use formality_types::grammar::{Parameter, Relation, RigidTy, TyData, Wcs};

use crate::{
    decls::Decls,
    prove::{prove, prove_after::prove_after, prove_normalize::prove_normalize},
};

use super::{constraints::Constraints, env::Env};

judgment_fn! {
    pub fn prove_sub(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        a: Parameter,
        b: Parameter,
    ) => Constraints {
        debug(a, b, assumptions, env)

        assert(a.kind() == b.kind())

        trivial(a == b => Constraints::none(env))

        (
            (prove_normalize(&decls, env, &assumptions, &x) => (c, y))
            (prove_after(&decls, c, &assumptions, Relation::sub(y, &z)) => c)
            ----------------------------- ("normalize-l")
            (prove_sub(decls, env, assumptions, x, z) => c)
        )

        (
            (prove_normalize(&decls, env, &assumptions, &y) => (c, z))
            (prove_after(&decls, c, &assumptions, Relation::sub(&x, &z)) => c)
            ----------------------------- ("normalize-r")
            (prove_sub(decls, env, assumptions, x, y) => c)
        )

        (
            (let RigidTy { name: a_name, parameters: a_parameters } = a)
            (let RigidTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)!
            (prove(decls, env, assumptions, Wcs::all_sub(a_parameters, b_parameters)) => c)
            ----------------------------- ("rigid")
            (prove_sub(decls, env, assumptions, TyData::RigidTy(a), TyData::RigidTy(b)) => c)
        )

        // FIXME: uncomment this when adding prove_outlives
        //(
        //    (prove_outlives(decls, env, assumptions, a, b) => c)
        //    ----------------------------- ("lifetime => outlives")
        //    (prove_sub(decls, env, assumptions, a: Lt, b: Lt) => c)
        //)
    }
}
