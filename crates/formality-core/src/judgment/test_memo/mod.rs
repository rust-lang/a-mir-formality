//! Tests for judgment-result memoization.
//!
//! Memoized entries live only within one top-level judgment call:
//!
//! * `iteration` tests entries reused only within the current fixed-point
//!   iteration. These entries must be discarded whenever an enclosing
//!   approximation grows.
//! * `root_scope` tests that returning from the top-level judgment discards
//!   both its own table and all completed descendant entries.
//!
//! This module contains the small judgments shared by the focused suites. Each
//! one isolates a particular cache-identity or invalidation case.

#![cfg(test)]

use std::cell::Cell;

use crate::{judgment_fn, ProvenSet, Set};

mod iteration;
mod root_scope;

// These counters are observability probes. A memo hit has the same semantic
// result as ordinary execution, so the tests count judgment-body executions to
// distinguish reuse from recomputation. The memo context clears itself when a
// root returns, so each test only resets the counters it observes.
thread_local! {
    static INVALIDATED_CHILD_EXECUTIONS: Cell<usize> = const { Cell::new(0) };
    static FAILED_CHILD_EXECUTIONS: Cell<usize> = const { Cell::new(0) };
    static CROSS_TYPE_EXECUTIONS: Cell<usize> = const { Cell::new(0) };
    static IDENTITY_LEFT_EXECUTIONS: Cell<usize> = const { Cell::new(0) };
    static IDENTITY_RIGHT_EXECUTIONS: Cell<usize> = const { Cell::new(0) };
}

fn values<T>(proven: ProvenSet<T>) -> Set<T>
where
    T: Clone + std::fmt::Debug + Ord,
{
    proven.iter().map(|(value, _)| value).collect()
}

// This mutually recursive pair has different output types. It checks that one
// erased memo context can safely hold entries for both judgments while their
// fixed points grow together.
judgment_fn! {
    fn cross_type_a() => u32 {
        debug()

        (
            --- ("base")
            (cross_type_a() => 0)
        )

        (
            (cross_type_b() => flag)
            (cross_type_b() => _)
            (if *flag)!
            --- ("from b")
            (cross_type_a() => 1)
        )
    }
}

judgment_fn! {
    fn cross_type_b() => bool {
        debug()

        (
            (let () = CROSS_TYPE_EXECUTIONS.set(
                CROSS_TYPE_EXECUTIONS.get() + 1,
            ))
            --- ("base")
            (cross_type_b() => false)
        )

        (
            (cross_type_a() => value)
            (if *value == 0)!
            --- ("from a")
            (cross_type_b() => true)
        )
    }
}

// These judgments have identical Rust signatures but different generated
// judgment identities. Their entries must never collide in an erased table.
judgment_fn! {
    fn identity_left(value: u32) => u32 {
        debug(value)

        (
            (let () = IDENTITY_LEFT_EXECUTIONS.set(
                IDENTITY_LEFT_EXECUTIONS.get() + 1,
            ))
            --- ("left")
            (identity_left(value) => *value + 1)
        )
    }
}

judgment_fn! {
    fn identity_right(value: u32) => u32 {
        debug(value)

        (
            (let () = IDENTITY_RIGHT_EXECUTIONS.set(
                IDENTITY_RIGHT_EXECUTIONS.get() + 1,
            ))
            --- ("right")
            (identity_right(value) => *value + 2)
        )
    }
}

judgment_fn! {
    fn calls_same_signature_judgments(value: u32) => u32 {
        debug(value)

        (
            (identity_left(value) => left)
            (identity_left(value) => _)
            (identity_right(value) => right)
            (identity_right(value) => _)
            --- ("both")
            (calls_same_signature_judgments(value) => *left + *right)
        )
    }
}

// Failed judgments carry diagnostics that are not represented in a proof map.
// Re-executing this judgment is therefore required: caching its empty result
// would incorrectly turn a contextual failure into a reusable negative fact.
judgment_fn! {
    fn failed_child() => () {
        debug()

        (
            (let () = FAILED_CHILD_EXECUTIONS.set(
                FAILED_CHILD_EXECUTIONS.get() + 1,
            ))
            (if false)
            --- ("failure")
            (failed_child() => ())
        )
    }
}

judgment_fn! {
    fn calls_failed_child_twice() => () {
        debug()

        (
            (let first = failed_child())
            (let second = failed_child())
            (if !first.is_proven() && !second.is_proven())
            --- ("twice")
            (calls_failed_child_twice() => ())
        )
    }
}

// `invalidation_b` first completes using the initial approximation of `a`.
// When `a` grows, its enclosing iteration is invalidated and `b` must execute
// again rather than reuse the result computed under the older approximation.
judgment_fn! {
    fn invalidation_a() => u32 {
        debug()

        (
            --- ("base")
            (invalidation_a() => 0)
        )

        (
            (invalidation_b() => value)
            (invalidation_b() => _)
            --- ("from b")
            (invalidation_a() => value)
        )
    }
}

judgment_fn! {
    fn invalidation_b() => u32 {
        debug()

        (
            (let () = INVALIDATED_CHILD_EXECUTIONS.set(
                INVALIDATED_CHILD_EXECUTIONS.get() + 1,
            ))
            --- ("base")
            (invalidation_b() => 1)
        )

        (
            (invalidation_a() => value)
            (if *value == 0)!
            --- ("after a grows")
            (invalidation_b() => 2)
        )
    }
}
