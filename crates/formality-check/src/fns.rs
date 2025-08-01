use formality_prove::Env;
use formality_rust::{
    grammar::{Fn, FnBoundData, MaybeFnBody},
    prove::ToWcs,
};
use formality_types::grammar::{CrateId, Fallible, Wcs};

use crate::Check;

impl Check<'_> {
    /// A "free function" is a free-standing function that is not part of an impl.
    pub(crate) fn check_free_fn(&self, f: &Fn, crate_id: &CrateId) -> Fallible<()> {
        self.check_fn(&Env::default(), Wcs::t(), f, crate_id)
    }

    /// Invoked for both free functions and methods.
    ///
    /// # Parameters
    ///
    /// * `in_env` -- the environment from the enclosing impl (if any)
    /// * `in_assumptions` -- where-clauses from the enclosing impl (if any)
    /// * `f` -- the function definition
    pub(crate) fn check_fn(
        &self,
        in_env: &Env,
        in_assumptions: impl ToWcs,
        f: &Fn,
        crate_id: &CrateId,
    ) -> Fallible<()> {
        let in_assumptions = in_assumptions.to_wcs();

        // We do not expect to have any inference variables in the where clauses.
        //
        // e.g., `impl<T: Ord> Foo<T> { ... }` would have a reference to a
        // universal variable `T`, not an existential one.
        assert!(in_env.only_universal_variables() && in_env.encloses((&in_assumptions, f)));

        // Create a mutable copy of the environment which we will extend
        // with the generic parameters from the function.
        let mut env = in_env.clone();

        // Instantiate the generic parameters declared on the fn
        // with universal variables (i.e., treat them as fresh,
        // unknown types).
        let Fn { id: _, binder } = f;
        let FnBoundData {
            input_tys,
            output_ty,
            where_clauses,
            body,
        } = env.instantiate_universally(binder);

        // The in-scope assumtion are the union of the assumptions from
        // the impl and the fn.
        let fn_assumptions: Wcs = (in_assumptions, &where_clauses).to_wcs();

        // All of the following must be well-formed:
        // where-clauses, input parameter types, and output type.
        self.prove_where_clauses_well_formed(&env, &fn_assumptions, &where_clauses)?;
        for input_ty in &input_tys {
            self.prove_goal(&env, &fn_assumptions, input_ty.well_formed())?;
        }
        self.prove_goal(&env, &fn_assumptions, &output_ty.well_formed())?;

        // Type-check the function body, if present.
        match body {
            MaybeFnBody::NoFnBody => {
                // No fn body occurs trait definitions only.
            }
            MaybeFnBody::FnBody(fn_body) => match fn_body {
                formality_rust::grammar::FnBody::TrustedFnBody => {
                    // A trusted function body is assumed to be valid, all set.
                }
                formality_rust::grammar::FnBody::MiniRust(body) => {
                    self.check_body(&env, &output_ty, &fn_assumptions, body, input_tys, crate_id)?;
                }
            },
        }

        Ok(())
    }
}
