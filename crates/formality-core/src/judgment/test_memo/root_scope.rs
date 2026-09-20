//! Root-scoped memoization tests.
//!
//! Completed judgments may be reused by other calls made while the same
//! top-level judgment is active. Once that root returns, its iteration table is
//! discarded: neither the root's result nor any completed descendant can be
//! observed by a later top-level call.

use std::cell::Cell;

use crate::judgment_fn;

use super::*;

thread_local! {
    static ROOT_EXECUTIONS: Cell<usize> = const { Cell::new(0) };
    static SHARED_CHILD_EXECUTIONS: Cell<usize> = const { Cell::new(0) };
}

judgment_fn! {
    fn counted_root(value: u32) => u32 {
        debug(value)

        (
            (let () = ROOT_EXECUTIONS.set(ROOT_EXECUTIONS.get() + 1))
            --- ("value")
            (counted_root(value) => value)
        )
    }
}

judgment_fn! {
    fn shared_child() => u32 {
        debug()

        (
            (let () = SHARED_CHILD_EXECUTIONS.set(
                SHARED_CHILD_EXECUTIONS.get() + 1,
            ))
            --- ("value")
            (shared_child() => 22)
        )
    }
}

judgment_fn! {
    fn first_root() => u32 {
        debug()

        (
            (shared_child() => value)
            --- ("child")
            (first_root() => value)
        )
    }
}

judgment_fn! {
    fn second_root() => u32 {
        debug()

        (
            (shared_child() => value)
            --- ("child")
            (second_root() => value)
        )
    }
}

#[test]
fn top_level_result_does_not_survive_its_call() {
    ROOT_EXECUTIONS.set(0);

    assert_eq!(values(counted_root(22)), crate::set![22]);
    assert_eq!(values(counted_root(22)), crate::set![22]);

    assert_eq!(ROOT_EXECUTIONS.get(), 2);
}

#[test]
fn completed_descendant_does_not_survive_its_root() {
    SHARED_CHILD_EXECUTIONS.set(0);

    assert_eq!(values(first_root()), crate::set![22]);
    assert_eq!(values(second_root()), crate::set![22]);

    assert_eq!(SHARED_CHILD_EXECUTIONS.get(), 2);
}
