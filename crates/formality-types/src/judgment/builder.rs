use std::sync::Arc;

use crate::cast::{DowncastFrom, Upcast};

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
        M: DowncastFrom<J>,
        R: IntoIterator<Item = O>,
        O: Upcast<J::Output>,
    {
        let rule = move |input: &J| -> Vec<J::Output> {
            match M::downcast_from(input) {
                Some(m) => closure(m).into_iter().map(|o1| o1.upcast()).collect(),
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
