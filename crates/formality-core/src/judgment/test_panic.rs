#![cfg(test)]

use std::cell::Cell;

use crate::judgment_fn;

use super::memo;

thread_local! {
    static PANIC_ON_BASE_CASE: Cell<bool> = const { Cell::new(true) };
}

// The `true` case opens a recursive judgment and memo iteration, then calls the
// `false` base case. The base case panics only on its first execution. This
// forces both the active-judgment guard and the memo iteration guard to unwind
// through a nested call. Calling the judgment again after catching the panic
// checks that neither guard left stale thread-local state behind.
judgment_fn! {
    fn panic_during_recursion(recurse: bool) => () {
        debug(recurse)

        (
            (panic_during_recursion(false) => ())
            --- ("recursive")
            (panic_during_recursion(true) => ())
        )

        (
            (let () = PANIC_ON_BASE_CASE.with(|flag| {
                assert!(!flag.replace(false), "panic from judgment body");
            }))
            --- ("base")
            (panic_during_recursion(false) => ())
        )
    }
}

#[test]
fn panic_does_not_leave_an_active_judgment_behind() {
    use std::panic::{catch_unwind, AssertUnwindSafe};

    PANIC_ON_BASE_CASE.set(true);

    let panic = catch_unwind(AssertUnwindSafe(|| panic_during_recursion(true)));
    assert!(panic.is_err());
    assert_eq!(memo::active_iteration_count(), 0);

    let result = panic_during_recursion(true);
    assert!(result.is_proven(), "{result}");
}
