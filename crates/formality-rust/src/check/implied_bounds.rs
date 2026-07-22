use crate::grammar::{InputArg, Parameter, RigidName, RigidTy, Ty, Wcs, WhereClause};
use formality_core::{judgment_fn, Cons};

judgment_fn! {
    pub fn implied_bounds_from_fn(
        args: Vec<InputArg>,
        output: Ty,
    ) => Wcs {
        debug(args, output)

        (
            (implied_bounds_from_args(args) => args_wcs)
            (implied_bounds_from_ty(output) => output_wcs)
            ------------------------------------------------------------ ("bounds")
            (implied_bounds_from_fn(args, output) => (args_wcs, output_wcs))
        )
    }

}

judgment_fn! {
    fn implied_bounds_from_args(
        tys: Vec<InputArg>
    ) => Wcs {
        debug(tys)

        (
            (implied_bounds_from_ty(arg.ty.clone()) => head_wcs)
            (implied_bounds_from_args(tail) => tail_wcs)
            ------------------------------------------------------------ ("recurse args")
            (implied_bounds_from_args(Cons(arg, tail)) => (head_wcs, tail_wcs))
        )

        (
            ------------------------------------------------------------ ("nil")
            (implied_bounds_from_args(()) => ())
        )
    }

}

judgment_fn! {
    fn implied_bounds_from_ty(
        ty: Ty,
    ) => Wcs {
        debug(ty)

        (
            (if let Some(Parameter::Lt(lt)) = parameters.get(0))
            (if let Some(inner) = parameters.get(1))
            (let wc = vec![WhereClause::Outlives((*inner).clone(), (**lt).clone())])
            (implied_bounds_from_params(vec![inner]) => inner_wcs)
            ------------------------------------------------------------ ("inner ref")
            (implied_bounds_from_ty(RigidTy { name: RigidName::Ref(_), parameters }, ) => (inner_wcs, wc))
        )

        (
            (implied_bounds_from_params(parameters) => wcs)
            ------------------------------------------------------------ ("inner T")
            (implied_bounds_from_ty(RigidTy { name: _, parameters }) => wcs)
        )

        (
            ------------------------------------------------------------ ("no bounds")
            (implied_bounds_from_ty(ty) => ())
        )
    }

}

judgment_fn! {
    fn implied_bounds_from_params(
        param: Vec<Parameter>
    ) => Wcs {
        debug(param)

        (
            (implied_bounds_from_ty(ty) => head_wcs)
            (implied_bounds_from_params(tail) => tail_wcs)
            ------------------------------------------------------------ ("recurse ty param")
            (implied_bounds_from_params(Cons(Parameter::Ty(ty), tail)) => (head_wcs, tail_wcs))
        )

        (
            (implied_bounds_from_params(tail) => tail_wcs)
            ------------------------------------------------------------ ("param not ty")
            (implied_bounds_from_params(Cons(_, tail)) => tail_wcs)
        )

        (
            ------------------------------------------------------------ ("nil")
            (implied_bounds_from_params(_x) => ())
        )
    }
}
