use formality_types::{
    cast::Upcast,
    grammar::{AliasTy, AtomicPredicate, AtomicRelation, Parameter, TraitRef, APR},
};

use crate::grammar::Program;

impl formality_logic::Database for Program {
    fn program_clauses(
        &self,
        _predicate: &AtomicPredicate,
    ) -> Vec<formality_types::grammar::ProgramClause> {
        self.to_clauses()
    }

    fn invariants_for_apr(&self, _predicate: &APR) -> Vec<formality_types::grammar::Invariant> {
        self.to_invariants()
    }

    fn force_ambiguous(&self, env: &formality_logic::Env, apr: &APR) -> bool {
        match apr {
            APR::AtomicPredicate(predicate) => match predicate {
                AtomicPredicate::IsImplemented(TraitRef {
                    trait_id: _,
                    parameters,
                })
                | AtomicPredicate::HasImpl(TraitRef {
                    trait_id: _,
                    parameters,
                })
                | AtomicPredicate::NormalizesTo(
                    AliasTy {
                        name: _,
                        parameters,
                    },
                    _,
                ) => env.is_unbound_inference_variable(&parameters[0]),

                AtomicPredicate::WellFormedTy(ty) => {
                    let p: Parameter = ty.upcast();
                    env.is_unbound_inference_variable(&p)
                }

                AtomicPredicate::WellFormedTraitRef(_) => false,
            },
            APR::AtomicRelation(relation) => match relation {
                AtomicRelation::Equals(_, _)
                | AtomicRelation::Sub(_, _)
                | AtomicRelation::Outlives(_, _) => false,
            },
        }
    }
}
