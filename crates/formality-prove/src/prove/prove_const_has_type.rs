use formality_core::judgment_fn;
use formality_types::grammar::{Const, ScalarValue, Ty, Wcs};

use crate::{
    decls::Decls,
    prove::{
        env::Env,
        prove_eq::prove_eq,
    },
};

use super::constraints::Constraints;

judgment_fn! {
    /// The "heart" of the trait system -- prove that a where-clause holds given a set of declarations, variable environment, and set of assumptions.
    /// If successful, returns the constraints under which the where-clause holds.
    pub fn prove_const_has_type(
        decls: Decls,
        env: Env,
        assumptions: Wcs,
        constant: Const,
        ty: Ty,
    ) => Constraints {
        debug(constant, ty, assumptions, env, decls)

        // (
        //     --- ("rigid constant")
        //     (prove_const_has_type(decls, env, assumptions, RigidConstData { name, parameters, values }, ty) => c)
        // )

        (
            (prove_eq(decls, env, assumptions, scalar.ty(), ty) => c)
            --- ("rigid constant")
            (prove_const_has_type(decls, env, assumptions, scalar: ScalarValue, ty) => c)
        )
    }
}
