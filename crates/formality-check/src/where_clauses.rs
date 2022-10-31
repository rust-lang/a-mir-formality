use fn_error_context::context;
use formality_infer::Env;
use formality_types::{
    cast::Upcast,
    grammar::{AtomicPredicate, Fallible, Hypothesis, Predicate, PredicateData},
};

impl super::Check<'_> {
    pub(crate) fn prove_where_clauses_well_formed(
        &self,
        env: &Env,
        assumptions: impl Upcast<Vec<Hypothesis>>,
        where_clauses: &[Predicate],
    ) -> Fallible<()> {
        let assumptions = assumptions.upcast();
        for where_clause in where_clauses {
            self.prove_where_clause_well_formed(env, &assumptions, where_clause)?;
        }
        Ok(())
    }

    #[context("prove_where_clause_well_formed({assumptions:?} => {where_clause:?}")]
    fn prove_where_clause_well_formed(
        &self,
        env: &Env,
        assumptions: &[Hypothesis],
        where_clause: &Predicate,
    ) -> Fallible<()> {
        match where_clause.data() {
            PredicateData::AtomicPredicate(p) => {
                self.prove_atomic_predicate_well_formed(env, assumptions, p)
            }
            PredicateData::AtomicRelation(_) => todo!(),
            PredicateData::ForAll(_) => todo!(),
            PredicateData::Implies(_, _) => todo!(),
        }
    }

    fn prove_atomic_predicate_well_formed(
        &self,
        env: &Env,
        assumptions: &[Hypothesis],
        predicate: &AtomicPredicate,
    ) -> Fallible<()> {
        match predicate {
            AtomicPredicate::IsImplemented(trait_ref) => {
                self.prove_goal(env, assumptions, trait_ref.well_formed())?;
            }
            AtomicPredicate::HasImpl(trait_ref) => todo!(),
            AtomicPredicate::NormalizesTo(alias_ty, ty) => todo!(),
            AtomicPredicate::WellFormedTy(ty) => todo!(),
            AtomicPredicate::WellFormedTraitRef(trait_ref) => todo!(),
        }
        Ok(())
    }
}
