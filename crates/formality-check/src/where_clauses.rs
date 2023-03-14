use formality_prove::Env;
use formality_rust::grammar::{WhereClause, WhereClauseData};
use formality_types::{
    cast::Upcast,
    grammar::{Fallible, Parameter, TraitRef},
};

impl super::Check<'_> {
    pub(crate) fn prove_where_clauses_well_formed(
        &self,
        env: &Env,
        assumptions: impl Upcast<Vec<WhereClause>>,
        where_clauses: &[WhereClause],
    ) -> Fallible<()> {
        let assumptions: Vec<WhereClause> = assumptions.upcast();
        for where_clause in where_clauses {
            self.prove_where_clause_well_formed(env, &assumptions, where_clause)?;
        }
        Ok(())
    }

    fn prove_where_clause_well_formed(
        &self,
        in_env: &Env,
        assumptions: impl Upcast<Vec<WhereClause>>,
        where_clause: &WhereClause,
    ) -> Fallible<()> {
        let assumptions: Vec<WhereClause> = assumptions.upcast();
        match where_clause.data() {
            WhereClauseData::IsImplemented(self_ty, trait_id, parameters) => self
                .prove_trait_ref_well_formed(
                    in_env,
                    assumptions,
                    trait_id.with(self_ty, parameters),
                ),
            WhereClauseData::Outlives(a, b) => {
                self.prove_parameter_well_formed(in_env, &assumptions, a)?;
                self.prove_parameter_well_formed(in_env, assumptions, b)
            }
            WhereClauseData::ForAll(binder) => {
                let mut e = in_env.clone();
                let wc = e.instantiate_universally(binder);
                self.prove_where_clause_well_formed(&e, assumptions, &wc)
            }
        }
    }

    fn prove_parameter_well_formed(
        &self,
        env: &Env,
        assumptions: impl Upcast<Vec<WhereClause>>,
        parameter: impl Upcast<Parameter>,
    ) -> Fallible<()> {
        let parameter: Parameter = parameter.upcast();
        self.prove_goal(env, assumptions, parameter.well_formed())
    }

    fn prove_trait_ref_well_formed(
        &self,
        env: &Env,
        assumptions: impl Upcast<Vec<WhereClause>>,
        trait_ref: impl Upcast<TraitRef>,
    ) -> Fallible<()> {
        let trait_ref: TraitRef = trait_ref.upcast();
        self.prove_goal(env, assumptions, trait_ref.well_formed())?;
        Ok(())
    }
}
