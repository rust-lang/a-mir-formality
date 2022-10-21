use std::collections::BTreeSet;

use super::{stack::JudgmentStack, InferenceRule, Judgment, JudgmentBuilder};

/// Dummy struct that exists to host methods, mostly.
#[derive(Debug)]
pub(super) struct JudgmentApply<'me, J>(pub(super) &'me J);

impl<J> JudgmentApply<'_, J>
where
    J: Judgment,
{
    #[tracing::instrument(level = "Debug", ret)]
    pub(super) fn apply(self) -> BTreeSet<J::Output> {
        let JudgmentApply(input) = self;

        if let Some(r) = Self::with_stack(|stack| stack.search(input)) {
            return r;
        }

        Self::with_stack(|stack| stack.push(input));

        let rules = Self::rules();

        loop {
            let outputs: BTreeSet<J::Output> =
                rules.iter().flat_map(|rule| rule.apply(input)).collect();
            if !Self::with_stack(|stack| stack.update_outputs(input, outputs)) {
                break;
            }
        }

        Self::with_stack(|stack| stack.pop(input))
    }

    fn with_stack<R>(f: impl FnOnce(&mut JudgmentStack<J>) -> R) -> R {
        let thread_local = J::stack();
        thread_local.with(|v| f(&mut *v.borrow_mut()))
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
