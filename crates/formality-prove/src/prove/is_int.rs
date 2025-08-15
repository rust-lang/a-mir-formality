use crate::prove::prove_normalize::prove_normalize;
use crate::Constraints;
use crate::Decls;
use crate::Env;
use formality_core::judgment_fn;
use formality_types::grammar::Parameter::{self};
use formality_types::grammar::RigidName;
use formality_types::grammar::RigidTy;
use formality_types::grammar::Wcs;

judgment_fn! {
    pub fn ty_is_int(
        _decls: Decls,
        env: Env,
        assumptions: Wcs,
        ty: Parameter,
    ) => Constraints {
        debug(assumptions, ty, env)
        // If the type that we are currently checking is rigid, check if it is an int.
        // If the type can be normalized, normalize until rigid then check if it is an int.
        // For the rest of the case, it should fail.

        (
            (prove_normalize(&decl, &env, &assumptions, ty) => (c1, p))
            (let assumptions = c1.substitution().apply(&assumptions))
            (ty_is_int(&decl, &env, assumptions, p) => c2)
            ----------------------------- ("alias_ty is int")
            (ty_is_int(decl, env, assumptions, ty) => c2)
        )

        (
            (if id.is_int())
            ----------------------------- ("rigid_ty is int")
            (ty_is_int(_decls, _env, _assumptions, RigidTy {name: RigidName::ScalarId(id), parameters: _}) => Constraints::none(env))
        )

    }
}
