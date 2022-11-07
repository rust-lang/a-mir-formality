use fn_error_context::context;
use formality_infer::Env;
use formality_types::{
    cast::{To, Upcast},
    grammar::{
        AtomicPredicate, AtomicRelation, Fallible, Hypothesis, Predicate, PredicateData, Ty, APR,
    },
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
            PredicateData::Atomic(apr) => {
                self.prove_atomic_predicate_well_formed(env, assumptions, apr)
            }
            PredicateData::ForAll(_) => todo!(),
            PredicateData::Implies(_, _) => todo!(),
        }
    }

    fn prove_atomic_predicate_well_formed(
        &self,
        env: &Env,
        assumptions: &[Hypothesis],
        predicate: &APR,
    ) -> Fallible<()> {
        match predicate {
            APR::AtomicPredicate(AtomicPredicate::IsImplemented(trait_ref)) => {
                self.prove_goal(env, assumptions, trait_ref.well_formed())?;
            }
            APR::AtomicPredicate(AtomicPredicate::NormalizesTo(alias_ty, ty)) => {
                self.prove_goal(env, assumptions, alias_ty.to::<Ty>().well_formed())?;
                self.prove_goal(env, assumptions, ty.well_formed())?;
            }
            APR::AtomicPredicate(AtomicPredicate::WellFormedTy(_ty)) => {}
            APR::AtomicPredicate(AtomicPredicate::WellFormedTraitRef(_trait_ref)) => {}
            APR::AtomicRelation(AtomicRelation::Outlives(a, b)) => {
                self.prove_goal(env, assumptions, a.well_formed())?;
                self.prove_goal(env, assumptions, b.well_formed())?;
            }
            APR::AtomicPredicate(AtomicPredicate::HasImpl(_))
            | APR::AtomicRelation(AtomicRelation::Equals(_, _))
            | APR::AtomicRelation(AtomicRelation::Sub(_, _)) => {
                panic!("predicate would never appear directly in program text")
            }
        }
        Ok(())
    }
}
