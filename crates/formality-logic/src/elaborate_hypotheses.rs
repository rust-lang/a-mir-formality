use std::collections::BTreeSet;

use formality_types::{
    self,
    from_into_term::{To, Upcast},
    grammar::{
        AtomicPredicate, Hypothesis, HypothesisData, Invariant, InvariantImplication, Substitution,
    },
};

use crate::Db;

mod test;

pub fn elaborate_hypotheses(db: &dyn Db, hypotheses: &[Hypothesis]) -> Vec<Hypothesis> {
    let mut set = BTreeSet::default();
    let mut stack: Vec<_> = hypotheses.iter().cloned().collect();

    while let Some(hypothesis) = stack.pop() {
        if set.insert(hypothesis.clone()) {
            stack.extend(elaborate_hypothesis(db, &hypothesis));
        }
    }

    // The result will be in a sorted, deterministic order, which is nice.
    set.iter().cloned().collect()
}

fn elaborate_hypothesis(db: &dyn Db, hypothesis: &Hypothesis) -> Vec<Hypothesis> {
    match hypothesis.data() {
        HypothesisData::AtomicPredicate(predicate) => db
            .invariants_for_predicate(predicate)
            .iter()
            .flat_map(|i| apply_invariant_to_predicate(db, i, predicate))
            .collect(),

        HypothesisData::AtomicRelation(relation) => db.elaborate_relation(relation),

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

        HypothesisData::CoherenceMode => vec![hypothesis.clone()],
    }
}

fn apply_invariant_to_predicate(
    _db: &dyn Db,
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
