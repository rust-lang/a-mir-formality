use crate::{
    check::borrow_check::{env::TypeckEnv, flow_state::FlowState, nll::borrow_check},
    grammar::{Const, Predicates, RigidConstData, RigidName, Ty},
};
use formality_core::{judgment_fn, Upcast};

use crate::prove::{decls::Program, env::Env};

use super::constraints::Constraints;

judgment_fn! {
    /// The "heart" of the trait system -- prove that a where-clause holds given a set of declarations, variable environment, and set of assumptions.
    /// If successful, returns the constraints under which the where-clause holds.
    pub fn prove_const_has_type(
        decls: Program,
        env: Env,
        assumptions: Predicates,
        constant: Const,
    ) => (Ty, Constraints) {
        debug(constant, assumptions, env, decls)

        // (
        //     --- ("rigid constant")
        //     (prove_const_has_type(decls, env, assumptions, RigidConstData { name, parameters, values }, ty) => c)
        // )

        (
            --- ("rigid constant")
            (prove_const_has_type(_decls, env, _assumptions, Const::Scalar(scalar)) => (scalar.ty(), Constraints::none(env)))
        )


        (
            (if let Some(ty) = rigid_tuple_const_ty(&data))
            --- ("tuple constant")
            (prove_const_has_type(_decls, env, _assumptions, Const::RigidValue(data)) => (ty, Constraints::none(env)))
        )

        (
            (borrow_check(TypeckEnv::for_const(env, decls), assumptions, FlowState::default(), block) => ())
            --- ("block")
            (prove_const_has_type(decls, env, assumptions, Const::Block(block)) => (Ty::unit(), Constraints::none(env)))
        )
    }
}

/// The type of a tuple const whose elements are scalars or nested tuples.
/// Returns `None` for other shapes (or if the arity doesn't match the values).
fn rigid_tuple_const_ty(data: &RigidConstData) -> Option<Ty> {
    let RigidName::Tuple(arity) = data.name else {
        return None;
    };
    if !data.parameters.is_empty() || data.values.len() != arity {
        return None;
    }
    let mut tys: Vec<Ty> = Vec::new();
    for value in &data.values {
        let ty = match value {
            Const::Scalar(scalar) => scalar.ty().upcast(),
            Const::RigidValue(inner) => rigid_tuple_const_ty(inner)?,
            _ => return None,
        };
        tys.push(ty);
    }
    Some(Ty::rigid(RigidName::Tuple(arity), tys))
}
