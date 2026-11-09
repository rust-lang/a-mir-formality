#![cfg(test)]

use std::cell::Cell;

use crate::judgment_fn;

thread_local! {
    static EXECUTIONS: Cell<usize> = const { Cell::new(0) };
}

judgment_fn! {
    fn proof_metadata_cycle() => u32 {
        debug()

        (
            (let () = EXECUTIONS.set(EXECUTIONS.get() + 1))
            --- ("base")
            (proof_metadata_cycle() => 0)
        )

        (
            (proof_metadata_cycle() => value)
            --- ("recursive")
            (proof_metadata_cycle() => value)
        )
    }
}

#[test]
fn cyclic_judgment_converges_by_value_and_retains_its_base_proof() {
    EXECUTIONS.set(0);

    let (value, proof) = proof_metadata_cycle().into_singleton().unwrap();

    assert_eq!(value, 0);
    assert_eq!(EXECUTIONS.get(), 2);
    assert_eq!(proof.rule_name, Some("base"));
}

thread_local! {
    static COUNT_SNAPSHOTS: std::cell::RefCell<Vec<crate::Set<u32>>> = const { std::cell::RefCell::new(Vec::new()) };
}

// The growing self-recursion example in the fixed-points chapter.
// ANCHOR: book_count_rules
judgment_fn! {
    fn count() => u32 {
        debug()

        (
            --- ("base")
            (count() => 0)
        )

        (
            (count() => n)
            (if *n < 2)
            --- ("successor")
            (count() => *n + 1)
        )

// ANCHOR_END: book_count_rules
        // Observe the active approximation once per pass, including the empty
        // first pass. This rule contributes no outputs of its own.
        (
            (let () = COUNT_SNAPSHOTS.with(|snapshots| {
                snapshots.borrow_mut().push(count().iter().map(|(value, _)| value).collect());
            }))
            (if false)
            --- ("record approximation")
            (count() => 0)
        )
// ANCHOR: book_count_end
    }
}
// ANCHOR_END: book_count_end

#[test]
fn book_count_example_reaches_its_least_fixed_point() {
    COUNT_SNAPSHOTS.with(|snapshots| snapshots.borrow_mut().clear());
    let values = count().proven_results().unwrap();
    assert_eq!(values, crate::set![0, 1, 2]);
    COUNT_SNAPSHOTS.with(|snapshots| {
        assert_eq!(
            *snapshots.borrow(),
            vec![
                crate::set![],
                crate::set![0],
                crate::set![0, 1],
                crate::set![0, 1, 2],
            ]
        );
    });
}

// A graph with a three-node cycle and a tail: 3 -> 0 -> 1 -> 2 -> 0.
// Every node of the cycle depends on the others, so the solver must iterate to a
// fixed point, and nothing reaches the tail node except itself.
judgment_fn! {
    fn cycle_edge(a: u32) => u32 {
        debug(a)

        (
            (if *a == 3)
            --- ("3 -> 0")
            (cycle_edge(a) => 0)
        )

        (
            (if *a == 0)
            --- ("0 -> 1")
            (cycle_edge(a) => 1)
        )

        (
            (if *a == 1)
            --- ("1 -> 2")
            (cycle_edge(a) => 2)
        )

        (
            (if *a == 2)
            --- ("2 -> 0")
            (cycle_edge(a) => 0)
        )
    }
}

judgment_fn! {
    fn cycle_reachable(a: u32) => u32 {
        debug(a)

        (
            --- ("self")
            (cycle_reachable(a) => a)
        )

        (
            (cycle_edge(a) => b)
            (cycle_reachable(b) => c)
            --- ("successor")
            (cycle_reachable(a) => c)
        )
    }
}

#[test]
fn every_node_of_a_cycle_reaches_the_whole_cycle() {
    for start in [0_u32, 1, 2] {
        let values = cycle_reachable(start).proven_results().unwrap();
        assert_eq!(values, crate::set![0, 1, 2], "from {start}");
    }
}

#[test]
fn a_tail_node_reaches_the_cycle_but_is_not_reached_by_it() {
    let values = cycle_reachable(3_u32).proven_results().unwrap();
    assert_eq!(values, crate::set![0, 1, 2, 3]);
}
