use formality_prove::Env;
use formality_rust::grammar::{Adt, AdtBoundData, Field, Variant};
use formality_types::grammar::Fallible;

impl super::Check<'_> {
    pub(super) fn check_adt(&self, adt: &Adt) -> Fallible<()> {
        let Adt { id: _, binder } = adt;

        let mut env = Env::default();

        let AdtBoundData {
            where_clauses,
            variants,
        } = env.instantiate_universally(binder);

        self.prove_where_clauses_well_formed(&env, &where_clauses, &where_clauses)?;

        // FIXME: check names are unique or integers from 0..n

        for Variant { name: _, fields } in &variants {
            for Field { name: _, ty } in fields {
                self.prove_goal(&env, &where_clauses, ty.well_formed())?;
            }
        }

        Ok(())
    }
}
