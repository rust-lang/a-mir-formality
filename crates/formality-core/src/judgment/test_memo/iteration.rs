//! Iteration-local memoization tests.
//!
//! Completed child judgments may be reused while an enclosing fixed-point
//! approximation is unchanged. Nested completed tables flow up to their parent
//! iteration, but all such entries become invalid as soon as an outer result
//! grows. Only positive results participate, and the generated judgment type
//!—not merely its input/output signature—identifies a memo table.

use std::cell::Cell;

use crate::judgment_fn;

use super::*;

thread_local! {
    static REUSED_CHILD_EXECUTIONS: Cell<usize> = const { Cell::new(0) };
    static GRANDCHILD_EXECUTIONS: Cell<usize> = const { Cell::new(0) };
}

judgment_fn! {
    fn propagated_grandchild() => u32 {
        debug()

        (
            (let () = GRANDCHILD_EXECUTIONS.set(
                GRANDCHILD_EXECUTIONS.get() + 1,
            ))
            --- ("value")
            (propagated_grandchild() => 22)
        )
    }
}

judgment_fn! {
    fn propagated_child() => u32 {
        debug()

        (
            (propagated_grandchild() => value)
            --- ("grandchild")
            (propagated_child() => value)
        )
    }
}

judgment_fn! {
    fn calls_child_then_grandchild() => u32 {
        debug()

        (
            (propagated_child() => child_value)
            (propagated_grandchild() => direct_value)
            (if child_value == direct_value)
            --- ("both")
            (calls_child_then_grandchild() => child_value)
        )
    }
}

judgment_fn! {
    fn reused_child(value: u32) => u32 {
        debug(value)

        (
            (let () = REUSED_CHILD_EXECUTIONS.set(
                REUSED_CHILD_EXECUTIONS.get() + 1,
            ))
            --- ("value")
            (reused_child(value) => value)
        )
    }
}

judgment_fn! {
    fn calls_reused_child_twice(value: u32) => u32 {
        debug(value)

        (
            (reused_child(value) => result)
            (reused_child(value) => _)
            --- ("twice")
            (calls_reused_child_twice(value) => result)
        )
    }
}

#[test]
fn reuses_a_completed_child_in_one_iteration() {
    REUSED_CHILD_EXECUTIONS.set(0);

    assert_eq!(values(calls_reused_child_twice(22)), crate::set![22]);
    assert_eq!(REUSED_CHILD_EXECUTIONS.get(), 1);
}

#[test]
fn propagates_completed_grandchildren_to_the_parent_iteration() {
    GRANDCHILD_EXECUTIONS.set(0);

    assert_eq!(values(calls_child_then_grandchild()), crate::set![22]);
    assert_eq!(GRANDCHILD_EXECUTIONS.get(), 1);
}

#[test]
fn invalidates_completed_children_when_outer_approximation_grows() {
    INVALIDATED_CHILD_EXECUTIONS.set(0);

    assert_eq!(values(invalidation_a()), crate::set![0, 1, 2]);
    assert_eq!(INVALIDATED_CHILD_EXECUTIONS.get(), 3);
}

#[test]
fn shares_iteration_context_across_cyclic_judgment_output_types() {
    CROSS_TYPE_EXECUTIONS.set(0);

    assert_eq!(values(cross_type_a()), crate::set![0, 1]);
    assert_eq!(CROSS_TYPE_EXECUTIONS.get(), 3);
}

#[test]
fn generated_identity_separates_same_signature_judgments() {
    IDENTITY_LEFT_EXECUTIONS.set(0);
    IDENTITY_RIGHT_EXECUTIONS.set(0);

    assert_eq!(values(calls_same_signature_judgments(21)), crate::set![45]);
    assert_eq!(IDENTITY_LEFT_EXECUTIONS.get(), 1);
    assert_eq!(IDENTITY_RIGHT_EXECUTIONS.get(), 1);
}

#[test]
fn does_not_cache_failed_children_as_negative_facts() {
    FAILED_CHILD_EXECUTIONS.set(0);

    assert!(calls_failed_child_twice().is_proven());
    assert_eq!(FAILED_CHILD_EXECUTIONS.get(), 2);
}
