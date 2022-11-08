use formality_macros::term;
use formality_types::grammar::{
    AtomicRelation, Binder, ElaboratedHypotheses, Goal, Hypothesis, HypothesisData, KindedVarIndex,
    Parameter, ParameterKind, PlaceholderVar, Variable, APR,
};

use crate::{extrude::Relationship, Env};

#[term]
pub(crate) struct PlaceholderBound {
    pub(crate) for_all: Binder<PlaceholderBoundData>,
}

#[term]
pub(crate) struct PlaceholderBoundData {
    pub(crate) conditions: Vec<Goal>,
    pub(crate) placeholder: Parameter,
    pub(crate) relationship: Relationship,
    pub(crate) bound: Parameter,
}

impl Env {
    pub(crate) fn find_placeholder_bounds(
        &self,
        assumptions: &ElaboratedHypotheses,
        placeholder: PlaceholderVar,
        relationship: Relationship,
    ) -> Vec<PlaceholderBound> {
        assumptions
            .iter()
            .filter_map(|a| self.to_placeholder_bound(a, vec![], vec![], placeholder, relationship))
            .collect()
    }

    fn to_placeholder_bound(
        &self,
        assumption: &Hypothesis,
        mut accumulated_for_all_names: Vec<KindedVarIndex>,
        mut accumulated_conditions: Vec<Goal>,
        placeholder: PlaceholderVar,
        relationship: Relationship,
    ) -> Option<PlaceholderBound> {
        match assumption.data() {
            HypothesisData::Atomic(APR::AtomicRelation(r)) => {
                if let Some((placeholder_param, bound_param)) =
                    self.could_bound_placeholder(r, placeholder, relationship)
                {
                    Some(PlaceholderBound {
                        for_all: Binder::new(
                            &accumulated_for_all_names,
                            PlaceholderBoundData {
                                conditions: accumulated_conditions,
                                placeholder: placeholder_param.clone(),
                                relationship,
                                bound: bound_param.clone(),
                            },
                        ),
                    })
                } else {
                    None
                }
            }
            HypothesisData::Atomic(APR::AtomicPredicate(_)) => None,
            HypothesisData::ForAll(binder) => {
                let (names, h) = binder.open();
                accumulated_for_all_names.extend(names);
                self.to_placeholder_bound(
                    &h,
                    accumulated_for_all_names,
                    accumulated_conditions,
                    placeholder,
                    relationship,
                )
            }
            HypothesisData::Implies(conditions, consequence) => {
                accumulated_conditions.extend(conditions.iter().cloned());
                self.to_placeholder_bound(
                    consequence,
                    accumulated_for_all_names,
                    accumulated_conditions,
                    placeholder,
                    relationship,
                )
            }
        }
    }

    fn could_bound_placeholder<'r>(
        &self,
        r: &'r AtomicRelation,
        placeholder: PlaceholderVar,
        relationship: Relationship,
    ) -> Option<(&'r Parameter, &'r Parameter)> {
        match (relationship, r) {
            (Relationship::SubtypeOf, AtomicRelation::Sub(a, b))
            | (Relationship::Outlives, AtomicRelation::Outlives(a, b))
                if self.could_eq_placeholder(placeholder, a) =>
            {
                Some((a, b))
            }
            (Relationship::SupertypeOf, AtomicRelation::Sub(a, b))
            | (Relationship::OutlivedBy, AtomicRelation::Outlives(a, b))
                if self.could_eq_placeholder(placeholder, b) =>
            {
                Some((b, a))
            }
            (Relationship::SubtypeOf, _)
            | (Relationship::SupertypeOf, _)
            | (Relationship::Outlives, _)
            | (Relationship::OutlivedBy, _) => None,
        }
    }

    fn could_eq_placeholder(&self, placeholder: PlaceholderVar, parameter: &Parameter) -> bool {
        if parameter.kind() != placeholder.kind {
            false
        } else if let Some(v) = parameter.as_variable() {
            match v {
                Variable::BoundVar(_) => true,
                Variable::PlaceholderVar(p) => placeholder == p,
                Variable::InferenceVar(i) => match &self.data(i).mapped_to {
                    Some(p) => self.could_eq_placeholder(placeholder, p),
                    None => self.data(i).universe >= placeholder.universe,
                },
            }
        } else {
            false
        }
    }
}
