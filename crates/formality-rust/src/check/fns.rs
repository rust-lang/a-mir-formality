use crate::check::borrow_check::env::TypeckEnv;
use crate::check::borrow_check::flow_state::FlowState;
use crate::check::borrow_check::nll::borrow_check;
use crate::grammar::{Binder, CrateId, Fallible, FnBody, MaybeFnBody, Wcs};
use crate::prove::prove::{Env, Program};
use crate::{
    grammar::{Fn, FnBoundData, InputArg, Ty},
    prove::ToWcs,
};
use formality_core::{judgment::ProofTree, judgment_fn};

/// Helper: instantiate the fn's generic parameters as universal variables,
/// returning the extended environment and the bound data.
fn instantiate_fn_env(in_env: &Env, binder: &Binder<FnBoundData>) -> (Env, FnBoundData) {
    let mut env = in_env.clone();
    let bound_data = env.instantiate_universally(binder);
    (env, bound_data)
}

/// Helper: check the function body. The borrow-checker dispatch stays as
/// a regular function call rather than a judgment.
fn check_fn_body(
    program: &Program,
    env: &Env,
    fn_assumptions: &Wcs,
    body: &MaybeFnBody,
    input_args: &[InputArg],
    output_ty: &Ty,
) -> Fallible<ProofTree> {
    match body {
        MaybeFnBody::NoFnBody => {
            // No fn body occurs in trait definitions only.
            Ok(ProofTree::leaf("no fn body"))
        }
        MaybeFnBody::FnBody(fn_body) => match fn_body {
            FnBody::TrustedFnBody => {
                // A trusted function body is assumed to be valid.
                Ok(ProofTree::leaf("trusted fn body"))
            }
            FnBody::Expr(block) => {
                let typeck_env = TypeckEnv::for_fn_body(env, program, output_ty);
                let initial_state = FlowState::for_fn_body(env, input_args)?;
                Ok(
                    borrow_check(typeck_env, fn_assumptions, initial_state, block)
                        .check_proven()?,
                )
            }
        },
    }
}

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
    /// * `in_env` -- the environment from the enclosing impl (if any)
    /// * `in_assumptions` -- where-clauses from the enclosing impl (if any)
    /// * `f` -- the function definition
    pub(crate) fn check_fn(
        program: Program,
        in_env: Env,
        in_assumptions: Wcs,
        f: Fn,
        crate_id: CrateId,
    ) => () {
        debug(f, crate_id, in_env, in_assumptions, program)
        assert(in_env.only_universal_variables() && in_env.encloses((in_assumptions, f)))

        (
            // Instantiate the generic parameters declared on the fn
            // with universal variables.
            (let (env, FnBoundData { input_args, output_ty, where_clauses, body }) =
                instantiate_fn_env(in_env, &f.binder))

            // The in-scope assumptions are the union of the assumptions
            // from the impl and the fn.
            (let fn_assumptions: Wcs = (in_assumptions, where_clauses).to_wcs())

            // All of the following must be well-formed:
            // where-clauses, input parameter types, and output type.
            (super::where_clauses::prove_where_clauses_well_formed(
                program, env, fn_assumptions, where_clauses) => ())
            (for_all(input_arg in input_args)
                (super::prove_goal(
                    program, env, fn_assumptions, input_arg.ty.well_formed()) => ()))
            (super::prove_goal(
                program, env, fn_assumptions, output_ty.well_formed()) => ())

            // Type-check the function body, if present.
            (check_fn_body(program, env, fn_assumptions, body, input_args, output_ty) => ())
            ------------------------------------------------------------ ("check fn")
            (check_fn(program, in_env, in_assumptions, f, crate_id) => ())
        )
    }
}
