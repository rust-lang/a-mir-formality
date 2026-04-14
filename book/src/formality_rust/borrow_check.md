# Borrow checking

Borrow checking lives under `check/borrow_check/`. Its top-level judgment is `borrow_check`, which checks that the loans issued while executing a basic block are respected.

## `borrow_check`

At the top level, borrow checking works over:

* a `TypeckEnv`
* a set of assumptions
* a `FlowState`
* a `Block`

`borrow_check` delegates to `borrow_check_block`, which walks the block and updates the flow state statement by statement.

## `FlowState`

`FlowState` is the flow-sensitive state threaded through borrow checking. It contains:

* `scopes`
* `current`
* `breaks`
* `continues`

Its `current` field is a `PointFlowState`, which tracks:

* `loans_live`
* `outlives`

## Loans

A `Loan` represents a borrow produced by an expression like `&'a place`. It records:

* the lifetime of the borrow
* the borrowed place
* the borrow kind

Borrow checking adds loans to the current flow state and checks later accesses against the live loans.

## Liveness

The borrow checker computes liveness with `LivePlaces` and the `LiveBefore` trait.

This is used to determine which places must be valid before a statement or expression, including at `break` and `continue` targets.

## Outlives

Borrow checking also accumulates pending outlives constraints, represented as `PendingOutlives { a, b }`.

The `outlives.rs` code verifies these constraints:

* existential variables succeed trivially
* universal variables must be justified by the assumptions
