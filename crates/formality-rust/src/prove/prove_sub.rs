use crate::grammar::{Goals, Lt, Parameter, Predicate, RigidTy, Ty};
use crate::prove::Constrained;
use formality_core::judgment_fn;

use crate::prove::prove_outlives::prove_outlives;
use crate::prove::{
    decls::Program, prove, prove_after::prove_after, prove_normalize::prove_normalize,
};

use super::{constraints::Constraints, env::Env};

judgment_fn! {
    pub fn prove_sub(
        _decls: Program,
        env: Env,
        assumptions: Goals,
        a: Parameter,
        b: Parameter,
    ) => Constraints {
        debug(a, b, assumptions, env)

        assert(a.kind() == b.kind())

        trivial(a == b => Constraints::none(env))

        (
            (prove_normalize(decls, env, assumptions, x) => Constrained(y, c))
            (prove_after(decls, c, assumptions, Predicate::sub(y, z)) => c)
            ----------------------------- ("normalize-l")
            (prove_sub(decls, env, assumptions, x, z) => c)
        )

        (
            (prove_normalize(decls, env, assumptions, y) => Constrained(z, c))
            (prove_after(decls, c, assumptions, Predicate::sub(x, &z)) => c)
            ----------------------------- ("normalize-r")
            (prove_sub(decls, env, assumptions, x, y) => c)
        )

        (
            (let RigidTy { name: a_name, parameters: a_parameters } = a)
            (let RigidTy { name: b_name, parameters: b_parameters } = b)
            (if a_name == b_name)!
            (prove(decls, env, assumptions, Goals::all_sub(a_parameters, b_parameters)) => c)
            ----------------------------- ("rigid")
            (prove_sub(decls, env, assumptions, Ty::RigidTy(a), Ty::RigidTy(b)) => c)
        )

        (
            (prove_outlives(decls, env, assumptions, a, b) => c)
            ----------------------------- ("lifetime => outlives")
            (prove_sub(decls, env, assumptions, a: Lt, b: Lt) => c)
        )
    }
}
