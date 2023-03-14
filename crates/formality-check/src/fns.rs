use formality_prove::Env;
use formality_rust::grammar::{Fn, FnBoundData, WhereClause};
use formality_types::{cast::Upcast, grammar::Fallible};

use crate::Check;

impl Check<'_> {
    pub(crate) fn check_free_fn(&self, _f: &Fn) -> Fallible<()> {
        todo!()
    }

    pub(crate) fn check_fn(
        &self,
        in_env: &Env,
        in_assumptions: &[WhereClause],
        f: &Fn,
    ) -> Fallible<()> {
        let mut env = in_env.clone();

        let Fn { id: _, binder } = f;

        let FnBoundData {
            input_tys,
            output_ty,
            where_clauses,
            body: _,
        } = env.instantiate_universally(binder);

        let assumptions: Vec<WhereClause> = (in_assumptions, &where_clauses).upcast();

        self.prove_where_clauses_well_formed(&env, &assumptions, &where_clauses)?;

        for input_ty in &input_tys {
            self.prove_goal(&env, &assumptions, input_ty.well_formed())?;
        }

        self.prove_goal(&env, &assumptions, output_ty.well_formed())?;

        Ok(())
    }
}
