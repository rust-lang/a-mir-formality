use formality_prove::Env;
use formality_rust::{
    grammar::{Fn, FnBoundData},
    prove::ToWcs,
};
use formality_types::{
    grammar::{Fallible, Wcs},
};

use crate::Check;

impl Check<'_> {
    pub(crate) fn check_free_fn(&self, f: &Fn) -> Fallible<()> {
        self.check_fn(&Env::default(), Wcs::t(), f)
    }

    pub(crate) fn check_fn(
        &self,
        in_env: &Env,
        in_assumptions: impl ToWcs,
        f: &Fn,
    ) -> Fallible<()> {
        let in_assumptions = in_assumptions.to_wcs();
        assert!(in_env.only_universal_variables() && in_env.encloses((&in_assumptions, f)));

        let mut env = in_env.clone();

        let Fn { id: _, binder } = f;

        let FnBoundData {
            input_tys,
            output_ty,
            where_clauses,
            body: _,
        } = env.instantiate_universally(binder);

        let fn_assumptions: Wcs = (in_assumptions, &where_clauses).to_wcs();

        self.prove_where_clauses_well_formed(&env, &fn_assumptions, &where_clauses)?;

        for input_ty in &input_tys {
            self.prove_goal(&env, &fn_assumptions, input_ty.well_formed())?;
        }

        self.prove_goal(&env, &fn_assumptions, output_ty.well_formed())?;

        Ok(())
    }
}
