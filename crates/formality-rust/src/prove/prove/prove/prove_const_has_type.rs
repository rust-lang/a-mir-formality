use crate::types::grammar::{minirust, Const, ScalarValue, Ty, Wcs};
use formality_core::judgment_fn;

use crate::prove::prove::{decls::Decls, prove::env::Env};

use super::constraints::Constraints;

judgment_fn! {
    /// The "heart" of the trait system -- prove that a where-clause holds given a set of declarations, variable environment, and set of assumptions.
    /// If successful, returns the constraints under which the where-clause holds.
    pub fn prove_const_has_type(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        constant: Const,
    ) => (Ty, Constraints) {
        debug(constant, assumptions, env, decls)

        // (
        //     --- ("rigid constant")
        //     (prove_const_has_type(decls, env, assumptions, RigidConstData { name, parameters, values }, ty) => c)
        // )

        (
            --- ("rigid constant")
            (prove_const_has_type(_decls, env, _assumptions, scalar: ScalarValue) => (scalar.ty(), Constraints::none(env)))
        )

        (
            (let minirust::Body { ret, params, .. } = body)
            (p in params)
            (if p.id == ret)
            --- ("rv to tsv")
            (prove_const_has_type(_decls, env, _assumptions, body: minirust::Body) => (p.ty, Constraints::none(&env)))
        )
    }
}
