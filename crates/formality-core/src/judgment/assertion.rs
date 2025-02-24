use std::fmt::Debug;

/// Helper trait for assertions in judgments.
/// For each `assert(x)`, we will invoke `JudgmentAssertion::assert`.
/// This allows us to support both booleans and results.
pub trait JudgmentAssertion {
    fn assert(self, expr: &str);
}

impl<E: Debug> JudgmentAssertion for Result<(), E> {
    #[track_caller]
    fn assert(self, expr: &str) {
        match self {
            Ok(()) => (),
            Err(e) => panic!("judgment assertion failed: `{expr}` got {e:?}"),
        }
    }
}

impl JudgmentAssertion for bool {
    #[track_caller]
    fn assert(self, expr: &str) {
        assert!(self, "judgment assertion failed: `{expr}` is false");
    }
}
