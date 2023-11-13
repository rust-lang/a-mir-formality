use fn_error_context::context;
use formality_core::Upcast;
use formality_prove::Env;
use formality_rust::{
    grammar::{WhereClause, WhereClauseData},
    prove::ToWcs,
};
use formality_types::grammar::{ConstData, Fallible, Parameter, Relation, TraitRef};

impl super::Check<'_> {
    pub(crate) fn prove_where_clauses_well_formed(
        &self,
        env: &Env,
        assumptions: impl ToWcs,
        where_clauses: &[WhereClause],
    ) -> Fallible<()> {
        for where_clause in where_clauses {
            self.prove_where_clause_well_formed(env, &assumptions, where_clause)?;
        }
        Ok(())
    }

    #[context("prove_where_clause_well_formed({where_clause:?})")]
    // FIXME(oli-obk): figure out why is this a function and not a `judgment_fn`.
    fn prove_where_clause_well_formed(
        &self,
        in_env: &Env,
        assumptions: impl ToWcs,
        where_clause: &WhereClause,
    ) -> Fallible<()> {
        match where_clause.data() {
            WhereClauseData::IsImplemented(self_ty, trait_id, parameters) => self
                .prove_trait_ref_well_formed(
                    in_env,
                    assumptions,
                    trait_id.with(self_ty, parameters),
                ),
            WhereClauseData::AliasEq(alias_ty, ty) => {
                self.prove_parameter_well_formed(in_env, &assumptions, alias_ty)?;
                self.prove_parameter_well_formed(in_env, &assumptions, ty)
            }
            WhereClauseData::Outlives(a, b) => {
                self.prove_parameter_well_formed(in_env, &assumptions, a)?;
                self.prove_parameter_well_formed(in_env, assumptions, b)
            }
            WhereClauseData::ForAll(binder) => {
                let mut e = in_env.clone();
                let wc = e.instantiate_universally(binder);
                self.prove_where_clause_well_formed(&e, assumptions, &wc)
            }
            WhereClauseData::TypeOfConst(ct, ty) => {
                match ct.data() {
                    ConstData::Value(_, t) => {
                        self.prove_goal(in_env, &assumptions, Relation::equals(ty, t))?
                    }
                    ConstData::Variable(_) => {}
                }
                // FIXME(oli-obk): prove that there is no `TypeOfConst` bound for a different type.
                self.prove_parameter_well_formed(in_env, &assumptions, ct.clone())?;
                self.prove_parameter_well_formed(in_env, assumptions, ty.clone())
            }
        }
    }

    fn prove_parameter_well_formed(
        &self,
        env: &Env,
        assumptions: impl ToWcs,
        parameter: impl Upcast<Parameter>,
    ) -> Fallible<()> {
        let parameter: Parameter = parameter.upcast();
        self.prove_goal(env, assumptions, parameter.well_formed())
    }

    fn prove_trait_ref_well_formed(
        &self,
        env: &Env,
        assumptions: impl ToWcs,
        trait_ref: impl Upcast<TraitRef>,
    ) -> Fallible<()> {
        let trait_ref: TraitRef = trait_ref.upcast();
        self.prove_goal(env, assumptions, trait_ref.well_formed())?;
        Ok(())
    }
}
