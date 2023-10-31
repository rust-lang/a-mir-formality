use super::Value;

pub struct FixedPointStack<Input, Output> {
    entries: Vec<StackEntry<Input, Output>>,
}

impl<Input, Output> Default for FixedPointStack<Input, Output> {
    fn default() -> Self {
        Self {
            entries: Default::default(),
        }
    }
}

struct StackEntry<Input, Output> {
    /// Input.
    input: Input,

    /// Current output, updated during computation as we approach a fixed point.
    output: Output,

    /// Initially false; set to true when the outputs of this rule
    /// are observed while it is being evaluated.
    has_dependents: bool,
}

impl<Input, Output> FixedPointStack<Input, Output>
where
    Input: Value,
    Output: Value,
{
    /// Access the top frame on the stack, which should be for `input`.
    fn top_frame(&mut self, input: &Input) -> &mut StackEntry<Input, Output> {
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
    pub fn search(&mut self, input: &Input) -> Option<Output> {
        for entry in &mut self.entries {
            if entry.input == *input {
                entry.has_dependents = true;
                return Some(entry.output.clone());
            }
        }

        None
    }

    /// Push an entry onto the stack, indicating it is currently being evaluated.
    /// There must not already be an entry for `input`.
    pub fn push(&mut self, input: &Input, output: Output) {
        assert!(self.search(input).is_none());

        self.entries.push(StackEntry {
            input: input.clone(),
            output,
            has_dependents: false,
        });
    }

    /// Add outputs to the top-most stack entry, which must be for `input`.
    /// Returns true if another iteration is needed before reaching a fixed point.
    pub fn update_output(&mut self, input: &Input, output: Output) -> bool {
        let top = self.top_frame(input);
        if top.output == output {
            return false;
        }

        top.output = output;
        top.has_dependents
    }

    /// Pops the top entry from the stack, returning the saved outputs.
    pub fn pop(&mut self, input: &Input) -> Output {
        let top = self.entries.pop().unwrap();
        assert_eq!(top.input, *input);
        top.output
    }
}
