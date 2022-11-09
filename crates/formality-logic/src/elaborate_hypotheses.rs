use std::collections::BTreeSet;

use formality_types::{
    self,
    cast::{To, Upcast},
    db::{Database, Db},
    grammar::{
        ElaboratedHypotheses, Hypothesis, HypothesisData, Invariant, InvariantImplication,
        Substitution, APR,
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
        HypothesisData::Atomic(apr) => db
            .invariants_for_apr(apr)
            .iter()
            .flat_map(|i| apply_invariant_to_predicate(db, i, apr))
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

fn apply_invariant_to_predicate(
    _db: &Db,
    invariant: &Invariant,
    predicate: &APR,
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
    invariant_predicate: &APR,
    predicate: &APR,
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
