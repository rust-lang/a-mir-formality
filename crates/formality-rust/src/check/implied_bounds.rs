use crate::{
    grammar::{AliasTy, InputArg, Parameter, RigidName, RigidTy, Ty, Wcs, WhereClause},
    prove::prove::{prove, AliasEqDeclBoundData, Env, Program},
};
use formality_core::{judgment_fn, Cons};

judgment_fn! {
    pub fn implied_bounds_from_fn(
        program: Program,
        env: Env,
        args: Vec<InputArg>,
        output: Ty,
    ) => Wcs {
        debug(args, output)

        (
            (implied_bounds_from_args(program, env, args) => args_wcs)
            (implied_bounds_from_ty(program, env, output) => output_wcs)
            ------------------------------------------------------------ ("bounds")
            (implied_bounds_from_fn(program, env, args, output) => (args_wcs, output_wcs))
        )
    }

}

judgment_fn! {
    fn implied_bounds_from_args(
        program: Program,
        env: Env,
        tys: Vec<InputArg>
    ) => Wcs {
        debug(tys)

        (
            (implied_bounds_from_ty(program, env, arg.ty.clone()) => head_wcs)
            (implied_bounds_from_args(program, env, tail) => tail_wcs)
            ------------------------------------------------------------ ("recurse args")
            (implied_bounds_from_args(program, env, Cons(arg, tail)) => (head_wcs, tail_wcs))
        )

        (
            ------------------------------------------------------------ ("nil")
            (implied_bounds_from_args(program, env, ()) => ())
        )
    }

}

judgment_fn! {
    fn implied_bounds_from_ty(
        program: Program,
        env: Env,
        ty: Ty,
    ) => Wcs {
        debug(ty)

        (
            (if let Some(Parameter::Lt(lt)) = parameters.get(0))
            (if let Some(inner) = parameters.get(1))
            (let wc = vec![WhereClause::Outlives((*inner).clone(), (**lt).clone())])
            (implied_bounds_from_params(program, env, vec![inner]) => inner_wcs)
            ------------------------------------------------------------ ("inner ref")
            (implied_bounds_from_ty(program, env, RigidTy { name: RigidName::Ref(_), parameters }, ) => (inner_wcs, wc))
        )

        (
            (implied_bounds_from_params(program, env, parameters) => wcs)
            ------------------------------------------------------------ ("inner T")
            (implied_bounds_from_ty(program, env, RigidTy { name: _, parameters }) => wcs)
        )

        (
            (implied_bounds_from_alias(program, env, alias) => wcs)
            ------------------------------------------------------------ ("alias type")
            (implied_bounds_from_ty(program, env, Ty::AliasTy(alias)) => wcs)
        )

        (
            ------------------------------------------------------------ ("no bounds")
            (implied_bounds_from_ty(program, env, ty) => ())
        )
    }

}

judgment_fn! {
    fn implied_bounds_from_params(
        program: Program,
        env: Env,
        param: Vec<Parameter>
    ) => Wcs {
        debug(param)

        (
            (implied_bounds_from_ty(program, env, ty) => head_wcs)
            (implied_bounds_from_params(program, env, tail) => tail_wcs)
            ------------------------------------------------------------ ("recurse ty param")
            (implied_bounds_from_params(program, env, Cons(Parameter::Ty(ty), tail)) => (head_wcs, tail_wcs))
        )

        (
            (implied_bounds_from_params(program, env, tail) => tail_wcs)
            ------------------------------------------------------------ ("param not ty")
            (implied_bounds_from_params(program, env, Cons(_, tail)) => tail_wcs)
        )

        (
            ------------------------------------------------------------ ("nil")
            (implied_bounds_from_params(program, env, _x) => ())
        )
    }
}

judgment_fn! {
    fn implied_bounds_from_alias(
        program: Program,
        env: Env,
        alias: AliasTy,
    ) => Wcs {
        debug(alias, env)

        (
            (decl in program.alias_eq_decls(&alias.name))
            (let (env, subst) = env.existential_substitution(&decl.binder))
            (let decl = decl.binder.instantiate_with(&subst).unwrap())
            (let AliasEqDeclBoundData { alias: AliasTy { name, parameters }, ty, where_clause: _ } = decl)
            (assert alias.name == *name)

            (prove(program, &env, Wcs::t(), Wcs::all_eq(&alias.parameters, &parameters)) => c)
            
            (let ty = c.substitution().apply(ty))

            (implied_bounds_from_ty(program, c.env(), &ty) => ty_wcs)
            ------------------------------------------------------------ ("alias implied bounds")
            (implied_bounds_from_alias(program, env, alias) => ty_wcs)
        )
    }
}
