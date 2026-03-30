use crate::check::borrow_check::env::TypeckEnv;
use crate::check::borrow_check::flow_state::FlowState;
use crate::check::borrow_check::nll::borrow_check;
use crate::check::prove_goal;
use crate::check::where_clauses::prove_where_clauses_well_formed;
use crate::grammar::{CrateId, FnBody, MaybeFnBody, Wcs};
use crate::prove::prove::{Env, Program};
use crate::{
    grammar::{Fn, FnBoundData},
    prove::ToWcs,
};
use formality_core::judgment_fn;

judgment_fn! {
    /// A "free function" is a free-standing function that is not part of an impl.
    pub(crate) fn check_free_fn(
        program: Program,
        f: Fn,
        crate_id: CrateId,
    ) => () {
        debug(f, crate_id, program)

        (
            (check_fn(program, Env::default(), Wcs::t(), f, crate_id) => ())
            ------------------------------------------------------------ ("check free fn")
            (check_free_fn(program, f, crate_id) => ())
        )
    }
}

judgment_fn! {
    /// Invoked for both free functions and methods.
    ///
    /// * `env` -- the environment from the enclosing impl (if any)
    /// * `assumptions` -- where-clauses from the enclosing impl (if any)
    /// * `f` -- the function definition
    pub(crate) fn check_fn(
        program: Program,
        env: Env,
        assumptions: Wcs,
        f: Fn,
        crate_id: CrateId,
    ) => () {
        debug(f, crate_id, env, assumptions, program)
        assert(env.only_universal_variables() && env.encloses((assumptions, f)))

        (
            // Instantiate the generic parameters declared on the fn
            // with universal variables.
            (let (env, FnBoundData { input_args, output_ty, where_clauses, body }) =
                env.instantiate_universally_into(&f.binder))

            // The in-scope assumptions are the union of the assumptions
            // from the impl and the fn.
            (let assumptions: Wcs = (assumptions, where_clauses).to_wcs())

            // All of the following must be well-formed:
            // where-clauses, input parameter types, and output type.
            (prove_where_clauses_well_formed(
                program, env, assumptions, where_clauses) => ())
            (for_all(input_arg in input_args)
                (prove_goal(
                    program, env, assumptions, input_arg.ty.well_formed()) => ()))
            (prove_goal(
                program, env, assumptions, output_ty.well_formed()) => ())

            // Type-check the function body, if present.
            (check_fn_body(program, env, assumptions, body, input_args, output_ty) => ())
            ------------------------------------------------------------ ("check fn")
            (check_fn(program, env, assumptions, f, crate_id) => ())
        )
    }
}

judgment_fn! {
    /// Check the function body, dispatching to the borrow checker for expression bodies.
    fn check_fn_body(
        program: Program,
        env: Env,
        assumptions: Wcs,
        body: MaybeFnBody,
        input_args: Vec<crate::grammar::InputArg>,
        output_ty: crate::grammar::Ty,
    ) => () {
        debug(body, assumptions, env, program)

        (
            // No fn body occurs in trait definitions only.
            ------------------------------------------------------------ ("no fn body")
            (check_fn_body(program, env, assumptions, MaybeFnBody::NoFnBody, input_args, output_ty) => ())
        )

        (
            // A trusted function body is assumed to be valid.
            ------------------------------------------------------------ ("trusted fn body")
            (check_fn_body(program, env, assumptions, MaybeFnBody::FnBody(FnBody::TrustedFnBody), input_args, output_ty) => ())
        )

        (
            // Type-check an expression body via the borrow checker.
            (let typeck_env = TypeckEnv::for_fn_body(env, program, output_ty))
            (let initial_state = FlowState::for_fn_body(env, input_args)?)
            (borrow_check(typeck_env, assumptions, initial_state, block) => ())
            ------------------------------------------------------------ ("expr fn body")
            (check_fn_body(program, env, assumptions, MaybeFnBody::FnBody(FnBody::Expr(block)), input_args, output_ty) => ())
        )
    }
}
