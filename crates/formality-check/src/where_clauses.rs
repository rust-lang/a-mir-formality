use fn_error_context::context;
use formality_infer::Env;
use formality_types::{
    cast::{To, Upcast},
    grammar::{AtomicPredicate, Fallible, Hypothesis, Predicate, PredicateData, Ty, APR},
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
            PredicateData::Atomic(APR::AtomicPredicate(p)) => {
                self.prove_atomic_predicate_well_formed(env, assumptions, p)
            }
            PredicateData::Atomic(APR::AtomicRelation(_)) => todo!(),
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
            AtomicPredicate::HasImpl(_trait_ref) => {
                panic!("predicate would never appear directly in program text")
            }
            AtomicPredicate::NormalizesTo(alias_ty, ty) => {
                self.prove_goal(env, assumptions, alias_ty.to::<Ty>().well_formed())?;
                self.prove_goal(env, assumptions, ty.well_formed())?;
            }
            AtomicPredicate::WellFormedTy(_ty) => {}
            AtomicPredicate::WellFormedTraitRef(_trait_ref) => {}
        }
        Ok(())
    }
}
