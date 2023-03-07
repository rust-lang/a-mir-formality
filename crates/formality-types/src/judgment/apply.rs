use std::collections::BTreeSet;

use crate::fixed_point::fixed_point;

use super::{InferenceRule, Judgment, JudgmentBuilder};

/// Dummy struct that exists to host methods, mostly.
#[derive(Debug)]
pub(super) struct JudgmentApply<'me, J>(pub(super) &'me J);

impl<J> JudgmentApply<'_, J>
where
    J: Judgment,
{
    pub(super) fn apply(self) -> BTreeSet<J::Output> {
        let JudgmentApply(input) = self;
        let rules = Self::rules();
        let result = fixed_point(
            J::tracing_span,
            J::stack(),
            input.clone(),
            |_| BTreeSet::default(),
            |input| rules.iter().flat_map(|rule| rule.apply(&input)).collect(),
        );
        result
    }

    fn rules() -> Vec<InferenceRule<J, J::Output>> {
        let mut builder = JudgmentBuilder::new();
        J::build_rules(&mut builder);
        builder.into_rules()
    }
}

impl<I, O> InferenceRule<I, O> {
    fn apply(&self, input: &I) -> Vec<O> {
        (self.closure)(input)
    }
}
