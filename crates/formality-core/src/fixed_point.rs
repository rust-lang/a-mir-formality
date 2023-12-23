use std::cell::RefCell;
use std::fmt::Debug;
use std::hash::Hash;
use std::thread::LocalKey;

mod stack;
pub use stack::FixedPointStack;

pub fn fixed_point<Input, Output>(
    tracing_span: impl Fn(&Input) -> tracing::Span,
    storage: &'static LocalKey<RefCell<FixedPointStack<Input, Output>>>,
    args: Input,
    default_value: impl Fn(&Input) -> Output,
    next_value: impl FnMut(Input) -> Output,
) -> Output
where
    Input: Value,
    Output: Value,
{
    stacker::maybe_grow(32 * 1024, 1024 * 1024, || {
        FixedPoint {
            tracing_span,
            storage,
            default_value,
            next_value,
        }
        .apply(args)
    })
}

struct FixedPoint<Input, Output, DefaultValue, NextValue, TracingSpan>
where
    Input: Value,
    Output: Value,
{
    tracing_span: TracingSpan,
    storage: &'static LocalKey<RefCell<FixedPointStack<Input, Output>>>,
    default_value: DefaultValue,
    next_value: NextValue,
}

pub trait Value: Clone + Eq + Debug + Hash + 'static {}
impl<T: Clone + Eq + Debug + Hash + 'static> Value for T {}

impl<Input, Output, DefaultValue, NextValue, TracingSpan>
    FixedPoint<Input, Output, DefaultValue, NextValue, TracingSpan>
where
    Input: Value,
    Output: Value,
    DefaultValue: Fn(&Input) -> Output,
    NextValue: FnMut(Input) -> Output,
    TracingSpan: Fn(&Input) -> tracing::Span,
{
    fn apply(&mut self, input: Input) -> Output {
        if let Some(r) = self.with_stack(|stack| stack.search(&input)) {
            tracing::debug!("recursive call to {:?}, yielding {:?}", input, r);
            return r;
        }

        self.with_stack(|stack| {
            let default_value = (self.default_value)(&input);
            stack.push(&input, default_value);
        });

        loop {
            let span = (self.tracing_span)(&input);
            let _guard = span.enter();
            let output = (self.next_value)(input.clone());
            tracing::debug!(?output);
            if !self.with_stack(|stack| stack.update_output(&input, output)) {
                break;
            } else {
                tracing::debug!("output is different from previous iteration, re-executing until fixed point is reached");
            }
        }

        self.with_stack(|stack| stack.pop(&input))
    }

    fn with_stack<R>(&self, f: impl FnOnce(&mut FixedPointStack<Input, Output>) -> R) -> R {
        self.storage.with(|v| f(&mut *v.borrow_mut()))
    }
}
