# Borrow checking

Borrow checking lives under `check/borrow_check/`. Its top-level judgment is `borrow_check`, which checks that the loans issued while executing a basic block are respected.

## `borrow_check`

{judgment}`borrow_check`

At the top level, borrow checking works over a `TypeckEnv`, a set of assumptions, a `FlowState`, and a `Block`. It delegates to `borrow_check_block`, which walks the block and updates the flow state statement by statement:

{judgment}`borrow_check_block`

## Statements

`borrow_check_statement` handles each kind of statement. Here are some representative rules:

### `let` bindings

{judgment-rule}`borrow_check_statement, let`

### `if` expressions

{judgment-rule}`borrow_check_statement, if`

### `loop`

{judgment-rule}`borrow_check_statement, loop`

### `break` and `continue`

{judgment-rule}`borrow_check_statement, break`

{judgment-rule}`borrow_check_statement, continue`

### `return`

{judgment-rule}`borrow_check_statement, return`

## Expressions

Borrow checking of value expressions is handled by `borrow_check_expr`:

{judgment}`borrow_check_expr`

## Outlives

Borrow checking also accumulates pending outlives constraints. The `outlives.rs` code verifies these constraints:

{judgment}`verify_universal_outlives`

Existential variables succeed trivially; universal variables must be justified by the assumptions:

{judgment}`only_assumed_outlives`
