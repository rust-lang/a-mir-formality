use std::cell::RefCell;
use std::fmt::Debug;
use std::hash::Hash;
use std::thread::LocalKey;

mod stack;
pub use stack::FixedPointStack;

pub fn fixed_point<Input, Output>(
    storage: &'static LocalKey<RefCell<FixedPointStack<Input, Output>>>,
    args: Input,
    default_value: impl Fn(&Input) -> Output,
    next_value: impl Fn(Input) -> Output,
) -> Output
where
    Input: Value,
    Output: Value,
{
    stacker::maybe_grow(32 * 1024, 1024 * 1024, || {
        FixedPoint {
            storage,
            default_value,
            next_value,
        }
        .apply(args)
    })
}

struct FixedPoint<Input: Value, Output: Value, DefaultValue, NextValue> {
    storage: &'static LocalKey<RefCell<FixedPointStack<Input, Output>>>,
    default_value: DefaultValue,
    next_value: NextValue,
}

pub trait Value: Clone + Eq + Debug + Hash + 'static {}
impl<T: Clone + Eq + Debug + Hash + 'static> Value for T {}

impl<Input, Output, DefaultValue, NextValue> FixedPoint<Input, Output, DefaultValue, NextValue>
where
    Input: Value,
    Output: Value,
    DefaultValue: Fn(&Input) -> Output,
    NextValue: Fn(Input) -> Output,
{
    fn apply(&self, input: Input) -> Output {
        if let Some(r) = self.with_stack(|stack| stack.search(&input)) {
            return r;
        }

        self.with_stack(|stack| {
            let default_value = (self.default_value)(&input);
            stack.push(&input, default_value);
        });

        loop {
            let output = (self.next_value)(input.clone());
            if !self.with_stack(|stack| stack.update_output(&input, output)) {
                break;
            }
        }

        self.with_stack(|stack| stack.pop(&input))
    }

    fn with_stack<R>(&self, f: impl FnOnce(&mut FixedPointStack<Input, Output>) -> R) -> R {
        self.storage.with(|v| f(&mut *v.borrow_mut()))
    }
}
