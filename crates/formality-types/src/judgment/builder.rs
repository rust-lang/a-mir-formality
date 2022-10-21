use std::sync::Arc;

use crate::{from_into_term::IntoTerm, matcher::Matcher};

use super::{InferenceRule, Judgment};

pub struct JudgmentBuilder<J: Judgment> {
    rules: Vec<InferenceRule<J, J::Output>>,
}

impl<J> JudgmentBuilder<J>
where
    J: Judgment,
{
    pub(super) fn new() -> Self {
        Self { rules: vec![] }
    }

    pub fn push_rule<M, R, O>(&mut self, closure: impl Fn(M) -> R + Send + 'static)
    where
        M: Matcher<J>,
        R: IntoIterator<Item = O>,
        O: IntoTerm<J::Output>,
    {
        let rule = move |input: &J| -> Vec<J::Output> {
            match M::try_match(input) {
                Some(m) => closure(m).into_iter().map(|o1| o1.into_term()).collect(),
                None => vec![],
            }
        };

        self.rules.push(InferenceRule {
            closure: Arc::new(rule),
        });
    }

    pub(super) fn into_rules(self) -> Vec<InferenceRule<J, J::Output>> {
        self.rules
    }
}
