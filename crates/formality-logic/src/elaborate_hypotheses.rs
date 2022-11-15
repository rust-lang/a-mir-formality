use std::collections::BTreeSet;

use crate::{Database, Db};
use formality_types::{
    self,
    cast::{To, Upcast, Upcasted},
    grammar::{
        AliasTy, AtomicPredicate, AtomicRelation, ElaboratedHypotheses, Hypothesis, HypothesisData,
        Invariant, InvariantImplication, ParameterData, RigidName, RigidTy, Substitution, TyData,
        APR,
    },
};

mod test;

pub fn elaborate_hypotheses<H>(
    db: &Db,
    hypotheses: impl IntoIterator<Item = H>,
) -> ElaboratedHypotheses
where
    H: Upcast<Hypothesis>,
{
    let mut set = BTreeSet::default();
    let mut stack: Vec<_> = hypotheses.into_iter().map(|h| h.upcast()).collect();

    while let Some(hypothesis) = stack.pop() {
        if set.insert(hypothesis.clone()) {
            stack.extend(elaborate_hypothesis(db, &hypothesis));
        }
    }

    ElaboratedHypotheses { set }
}

fn elaborate_hypothesis(db: &Db, hypothesis: &Hypothesis) -> Vec<Hypothesis> {
    match hypothesis.data() {
        HypothesisData::Atomic(APR::AtomicRelation(r)) => elaborate_relation(db, r),

        HypothesisData::Atomic(APR::AtomicPredicate(p)) => db
            .invariants_for_predicate(p)
            .iter()
            .flat_map(|i| apply_invariant_to_predicate(db, i, p))
            .collect(),

        HypothesisData::ForAll(binder) => {
            let (kinded_var_indices, hypothesis) = binder.open();
            elaborate_hypothesis(db, &hypothesis)
                .into_iter()
                .map(|h| Hypothesis::for_all(&kinded_var_indices, h))
                .collect()
        }

        HypothesisData::Implies(conditions, consequence) => elaborate_hypothesis(db, consequence)
            .into_iter()
            .map(|h| Hypothesis::implies(conditions, h))
            .collect(),
    }
}

fn elaborate_relation(_db: &Db, relation: &AtomicRelation) -> Vec<Hypothesis> {
    match relation {
        AtomicRelation::Equals(_, _) => vec![],
        AtomicRelation::Sub(_, _) => vec![],
        AtomicRelation::Outlives(a, b) => match a.data() {
            ParameterData::Ty(a) => match a {
                // If we know that (e.g.) `Vec<T>: b`, then we know that `T: b`
                TyData::RigidTy(RigidTy {
                    name: _,
                    parameters,
                }) => parameters
                    .iter()
                    .map(|a_p| a_p.outlives(b))
                    .upcasted()
                    .collect(),
                TyData::AliasTy(_) => vec![],
                TyData::PredicateTy(_) => vec![], // FIXME
                TyData::Variable(_) => vec![],
            },
            ParameterData::Lt(_) => vec![],
        },

        AtomicRelation::WellFormed(p) => match p.data() {
            // WF(Vec<T>) => WF[Vec](T)
            ParameterData::Ty(TyData::RigidTy(RigidTy {
                name: RigidName::AdtId(adt_id),
                parameters,
            })) => vec![adt_id.well_formed(parameters).upcast()],

            // WF(<T as Iterator>::Item) => WF[Iterator::Item](T)
            ParameterData::Ty(TyData::AliasTy(AliasTy { name, parameters })) => {
                vec![name.well_formed(parameters).upcast()]
            }

            // WF(&'a T) => T: 'a
            ParameterData::Ty(TyData::RigidTy(RigidTy {
                name: RigidName::Ref(_),
                parameters,
            })) => {
                assert_eq!(parameters.len(), 2);
                vec![parameters[1].outlives(&parameters[0]).upcast()]
            }

            ParameterData::Ty(_) => vec![],

            ParameterData::Lt(_) => vec![],
        },
    }
}

fn apply_invariant_to_predicate(
    _db: &Db,
    invariant: &Invariant,
    predicate: &AtomicPredicate,
) -> Option<Hypothesis> {
    invariant.assert();

    let (
        kinded_var_indices,
        InvariantImplication {
            condition,
            consequence,
        },
    ) = invariant.binder.open();

    let substitution = match_invariant_to_predicate(&condition, predicate)?;
    assert!({
        let domain = substitution.domain();
        kinded_var_indices
            .iter()
            .all(|kvi| domain.contains(&kvi.to()))
    });

    Some(substitution.apply(&consequence).upcast())
}

fn match_invariant_to_predicate(
    invariant_predicate: &AtomicPredicate,
    predicate: &AtomicPredicate,
) -> Option<Substitution> {
    let (invariant_skeleton, invariant_parameters) = invariant_predicate.debone();
    let (skeleton, parameters) = predicate.debone();
    if invariant_skeleton != skeleton {
        return None;
    }

    assert_eq!(invariant_parameters.len(), parameters.len());

    Some(
        invariant_parameters
            .iter()
            .zip(&parameters)
            .map(|(invariant_parameter, parameter)| {
                let var = invariant_parameter.as_variable().unwrap(); // the invariant validity invariant ensures this succeeds
                (var, parameter.clone())
            })
            .collect(),
    )
}
