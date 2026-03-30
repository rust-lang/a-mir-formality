use crate::{
    check::borrow_check::{env::TypeckEnv, flow_state::FlowState, nll::borrow_check},
    grammar::{Const, ConstData, Ty, Wcs},
};
use formality_core::judgment_fn;

use crate::prove::prove::{decls::Program, prove::env::Env};

use super::constraints::Constraints;

judgment_fn! {
    /// The "heart" of the trait system -- prove that a where-clause holds given a set of declarations, variable environment, and set of assumptions.
    /// If successful, returns the constraints under which the where-clause holds.
    pub fn prove_const_has_type(
        decls: Program,
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
            (prove_const_has_type(_decls, env, _assumptions, ConstData::Scalar(scalar)) => (scalar.ty(), Constraints::none(env)))
        )


        (
            (borrow_check(TypeckEnv::for_const(env, decls), assumptions, FlowState::default(), block) => ())
            --- ("block")
            (prove_const_has_type(decls, env, assumptions, ConstData::Block(block)) => (Ty::unit(), Constraints::none(env)))
        )
    }
}
