use std::collections::BTreeSet;

use super::Judgment;

pub struct JudgmentStack<J: Judgment> {
    entries: Vec<JudgmentStackEntry<J>>,
}

impl<J: Judgment> Default for JudgmentStack<J> {
    fn default() -> Self {
        Self {
            entries: Default::default(),
        }
    }
}

struct JudgmentStackEntry<J: Judgment> {
    /// Input.
    input: J,

    /// Current outputs. Initially empty, but updated with results during computation.
    outputs: BTreeSet<J::Output>,

    /// Initially false; set to true when the outputs of this rule
    /// are observed while it is being evaluated.
    has_dependents: bool,
}

impl<J: Judgment> JudgmentStack<J> {
    /// Access the top frame on the stack, which should be for `input`.
    fn top_frame(&mut self, input: &J) -> &mut JudgmentStackEntry<J> {
        let top = self.entries.last_mut().unwrap();
        assert_eq!(top.input, *input);
        top
    }

    /// Search backwards through the stack, looking for the given input.
    ///
    /// If it is found, return `Some` with the current outputs, and mark it
    /// as needing fixed point iteration.
    ///
    /// If not, return `None`.
    ///
    /// The fixed-point mark is returned when the stack is [popped](`Self::pop`) and is used
    /// as part of the fixed point algorithm.
    pub fn search(&mut self, input: &J) -> Option<BTreeSet<J::Output>> {
        for entry in &mut self.entries {
            if entry.input == *input {
                entry.has_dependents = true;
                return Some(entry.outputs.clone());
            }
        }

        None
    }

    /// Push an entry onto the stack, indicating it is currently being evaluated.
    /// There must not already be an entry for `input`.
    pub fn push(&mut self, input: &J) {
        assert!(self.search(input).is_none());

        self.entries.push(JudgmentStackEntry {
            input: input.clone(),
            outputs: BTreeSet::new(),
            has_dependents: false,
        });
    }

    /// Add outputs to the top-most stack entry, which must be for `input`.
    /// Returns true if another iteration is needed before reaching a fixed point.
    pub fn update_outputs(&mut self, input: &J, outputs: BTreeSet<J::Output>) -> bool {
        let top = self.top_frame(input);
        if top.outputs == outputs {
            return false;
        }

        top.outputs = outputs;
        return top.has_dependents;
    }

    /// Pops the top entry from the stack, returning the saved outputs.
    pub fn pop(&mut self, input: &J) -> BTreeSet<J::Output> {
        let top = self.entries.pop().unwrap();
        assert_eq!(top.input, *input);
        top.outputs
    }
}
