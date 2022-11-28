use formality_decl::grammar::{Adt, AdtBoundData, AdtVariant, Field};
use formality_logic::Env;
use formality_types::{
    cast::To,
    grammar::{Fallible, Hypothesis},
};

impl super::Check<'_> {
    pub(super) fn check_adt(&self, adt: &Adt) -> Fallible<()> {
        let Adt {
            kind: _,
            id: _,
            binder,
        } = adt;

        let mut env = Env::default();

        let AdtBoundData {
            where_clauses,
            variants,
        } = env.instantiate_universally(binder);

        let assumptions: Vec<Hypothesis> = where_clauses.to();

        self.prove_where_clauses_well_formed(&env, &assumptions, &where_clauses)?;

        // FIXME: check names are unique or integers from 0..n

        for AdtVariant { name: _, fields } in &variants {
            for Field { name: _, ty } in fields {
                self.prove_goal(&env, &assumptions, ty.well_formed())?;
            }
        }

        Ok(())
    }
}
