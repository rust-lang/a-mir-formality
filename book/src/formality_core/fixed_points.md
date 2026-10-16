# Fixed points and memoization

A judgment can call itself, directly or through other judgments. The runtime
must find all the values supported by finite proofs without following a cycle
forever. It must also know when it is safe to reuse a result instead of proving
it again.

This chapter explains the algorithm behind `judgment_fn!`. We use sets of output
values in the pseudocode and examples; [proof trees and failure diagnostics](#proofs-and-failures)
are handled alongside those sets.

## What recursive judgments mean

Judgments are **inductive**: a value is proven only if there is a finite proof
for it. A circular argument alone proves nothing. For example, if the only rule
for `A` says that `A` follows from `A`, then `A` has no proven outputs. Adding a
base case gives recursion something to build on.

The runtime starts each fresh call with an empty approximation of its outputs.
A recursive call with the same inputs reads that approximation. After evaluating
all the rules, the runtime may repeat the evaluation using the newly discovered
outputs. The intended result is the **least fixed point**: all the values that
can be derived starting from no facts.

Rules must be **monotone** in their recursive premises: learning more outputs
from a premise must not invalidate an output already derived. Testing that a
recursive premise has no answers and treating that as logical negation does not
satisfy this contract. An empty approximation means “nothing proven yet,” not
“this can never be proven.” The runtime checks that successive passes do not
lose outputs, but that check is not a substitute for designing monotone rules.

Termination also needs finite work: each rule pass must terminate, the search
must encounter only finitely many distinct judgment inputs, and each call must
have only finitely many possible outputs. Monotonicity alone does not prevent
an infinite sequence of distinct recursive calls or an endlessly growing set.

## Two kinds of stored results

A call is identified by the **judgment and its complete input tuple**. Two
different judgments with the same argument types are different keys.

During one top-level call, the runtime maintains a stack of active evaluations.
Each active evaluation owns:

- An approximation of its output values, initially empty.
- A flag recording whether a recursive call has read that approximation.
- A memo table containing completed descendants from its current rule pass.

These represent two different kinds of knowledge:

| Stored result | Meaning | How it is used |
| --- | --- | --- |
| Active approximation | Outputs discovered so far for a call still being evaluated | Break a recursive cycle by returning the current approximation |
| Completed memo entry | Outputs of a call that finished under the current enclosing approximations | Reuse a completed computation within the same root call |

“Completed” is relative to the enclosing approximations. A child can finish
while its parent is still discovering outputs. If that parent's approximation
changes, the child's completed result may become obsolete.

## The algorithm

Here is the normal execution path. `evaluate_rules_once` runs every applicable
rule, recursively using `solve` for judgment premises, and collects all outputs.
The actual implementation uses separate per-judgment active stacks and a shared
stack of memo tables; one combined stack makes the same lifecycle easier to see.

```text
solve(key):
    if key has an active frame:
        frame = that active frame
        frame.was_read_recursively = true
        return a snapshot of frame.outputs

    for frame in active_stack, from innermost to outermost:
        if key is in frame.completed:
            return frame.completed[key]

    frame = push_active_frame(
        key = key,
        outputs = {},
        was_read_recursively = false,
        completed = {}
    )

    loop:
        next = evaluate_rules_once(key)
        require frame.outputs is a subset of next
        grew = (next != frame.outputs)
        frame.outputs = next

        if not (grew and frame.was_read_recursively):
            break

        frame.completed.clear()

    pop_active_frame(frame)

    if there is a parent frame:
        merge frame.completed into parent.completed
        if frame.outputs is not empty:
            parent.completed[key] = frame.outputs
    else:
        discard frame.completed

    return frame.outputs
```

The active-call check comes **before** the memo lookup. Reading an active
approximation must record the recursive dependency, so growth will trigger
another pass. Memo lookup searches all enclosing tables, allowing work completed
by an earlier sibling to be reused inside a later sibling.

Outputs are updated after a whole rule pass, not after each rule. Thus a
recursive premise in a pass sees the approximation from the beginning of that
pass. The recursive-read flag stays set for the rest of this evaluation.

Growth alone does not require another pass. If nobody read this call's old
approximation, no recursive dependent needs its new outputs; one pass is enough.
Conversely, reading an approximation does not require another pass unless it
subsequently grows.

On completion, the runtime transfers both the call's own positive result and its
completed descendants to the parent. If two transferred entries have the same
key, their output sets must agree. At the root there is no parent, so all memo
entries are discarded. A later independent top-level call starts afresh.

The pseudocode shows normal returns. In the implementation, guards also remove
active frames and provisional memo tables when a rule panics, so an unwound call
does not contaminate a later evaluation.

## Example: building on a base case

This judgment is included directly from the test source. The test's observation
rule is omitted here so we can focus on the rules that produce outputs:

```rust,ignore
{{#include ../../../crates/formality-core/src/judgment/test_fixed_point.rs:book_count_rules}}
{{#include ../../../crates/formality-core/src/judgment/test_fixed_point.rs:book_count_end}}
```

Calling `count()` pushes an empty approximation. The premise `count()` in the
second rule finds that active call, marks its approximation as read, and returns
its current outputs.

| Pass | Approximation read by the recursive premise | Outputs after all rules | Next action |
| --- | --- | --- | --- |
| 1 | `{}` | `{0}` | Grew and was read: repeat |
| 2 | `{0}` | `{0, 1}` | Grew and was read: repeat |
| 3 | `{0, 1}` | `{0, 1, 2}` | Grew and was read: repeat |
| 4 | `{0, 1, 2}` | `{0, 1, 2}` | Unchanged: return |

The base case is available in the first pass's output, but not in the recursive
premise's snapshot until the second pass. Without the base rule, the first pass
would produce the empty set again and stop. That is how a cycle without a finite
proof fails.

The test
[`book_count_example_reaches_its_least_fixed_point`](https://github.com/rust-lang/a-mir-formality/blob/main/crates/formality-core/src/judgment/test_fixed_point.rs#L79)
asserts both the final `{0, 1, 2}` result and the four successive approximation
snapshots shown in the table. Its extra observation rule records those snapshots
without contributing any outputs.

## Example: why a completed child must be invalidated

These mutually recursive judgments come from the invalidation test. The execution
counter in `invalidation_b` is omitted from the excerpt:

```rust,ignore
{{#include ../../../crates/formality-core/src/judgment/test_memo/mod.rs:book_invalidation_a}}
{{#include ../../../crates/formality-core/src/judgment/test_memo/mod.rs:book_invalidation_b_start}}
{{#include ../../../crates/formality-core/src/judgment/test_memo/mod.rs:book_invalidation_b_end}}
```

Start with `invalidation_a()` as the root. Call its approximation **A** and the
answer from `invalidation_b()` **B** in the table below. When `invalidation_b`
executes, it reads `invalidation_a`'s active approximation. It can finish after
one pass: no recursive call reads its own approximation in this execution. Its
answer is nevertheless dependent on `invalidation_a`.

The two calls to `invalidation_b` in the same rule also exercise memoization:
the first computes the answer, and subsequent calls within that pass reuse it.

| Pass of `invalidation_a` | Active approximation A | Completed answer B | New answer A | Memo action |
| --- | --- | --- | --- | --- |
| 1 | `{}` | `{1}` | `{0, 1}` | Discard cached B before repeating |
| 2 | `{0, 1}` | `{1, 2}` | `{0, 1, 2}` | Discard cached B before repeating |
| 3 | `{0, 1, 2}` | `{1, 2}` | `{0, 1, 2}` | Converged; root discards the final table |

Keeping the first pass's cached `invalidation_b = {1}` would lose the output
`2`: the second pass would reuse that answer instead of letting `invalidation_b`
observe that `invalidation_a` now yields `0`. Clearing completed descendants on
every retry prevents this stale reuse. The runtime deliberately discards the
whole table; it does not track precisely which descendants read the changed
approximation.

If another judgment had called `invalidation_a`, the final pass's
`invalidation_b = {1, 2}` and the completed `invalidation_a = {0, 1, 2}` would
instead move into that caller's table.

The test
[`invalidates_completed_children_when_outer_approximation_grows`](https://github.com/rust-lang/a-mir-formality/blob/main/crates/formality-core/src/judgment/test_memo/iteration.rs#L110)
asserts the final `{0, 1, 2}` result and exactly three executions of
`invalidation_b`, one per pass in the table.

## Example: reuse between siblings

The shared leaf judgment returns `22`. Its execution counter is omitted here:

```rust,ignore
{{#include ../../../crates/formality-core/src/judgment/test_memo/iteration.rs:book_shared_leaf_start}}
{{#include ../../../crates/formality-core/src/judgment/test_memo/iteration.rs:book_shared_leaf_end}}
```

Two different judgments use that leaf as a premise:

```rust,ignore
{{#include ../../../crates/formality-core/src/judgment/test_memo/iteration.rs:book_left_child}}
{{#include ../../../crates/formality-core/src/judgment/test_memo/iteration.rs:book_right_child}}
```

The root calls both children in sequence:

```rust,ignore
{{#include ../../../crates/formality-core/src/judgment/test_memo/iteration.rs:book_sibling_root}}
```

1. `calls_sibling_children` and then `propagated_child` open their frames.
2. `propagated_grandchild` computes `{22}`. Its completion stores that answer
   in `propagated_child`'s table.
3. `propagated_child` completes. Its own result and its cached grandchild entry
   move into the root's table.
4. `propagated_other_child` opens a new frame. Its call to
   `propagated_grandchild` finds no active call and no entry in its own empty
   table, but finds `{22}` in the root's table. The leaf's rules do not execute
   again.
5. When `calls_sibling_children` finishes, its table is discarded. Calling it
   again recomputes the leaf; the cache does not persist between root calls.

If the root instead needed another fixed-point pass, it would clear these
entries before that pass. This applies even to entries such as this leaf result
that happen not to depend on the root.

The test
[`siblings_reuse_a_descendant_from_their_parent_table`](https://github.com/rust-lang/a-mir-formality/blob/main/crates/formality-core/src/judgment/test_memo/iteration.rs#L174)
checks that the shared leaf executes once within a root call and executes again
for a second independent root.

## Why the scopes make reuse valid

While a child executes, the active ancestors are suspended: their approximations
are fixed. A child's table contains only descendants completed under those
approximations and the child's current pass. Before the child changes the
approximation seen by another pass, it clears that table. Once the child
converges, its final pass's descendants remain valid under its final
approximation, so they can travel upward with its answer.

The same discipline applies at every level. An ancestor that grows clears all
completed entries accumulated below it before running another pass. Entries in
still-enclosing tables were completed under unchanged enclosing approximations;
they remain available to nested calls. This relies on monotone rules and on the
inputs identifying everything that affects the answer. Changing hidden mutable
state during a root call would violate that assumption.

## Proofs and failures

The implementation maps each output value to a proof tree. It keeps the smallest
proof observed for a value, but **only changes in output values drive iteration**.
A recursive rule can construct a different or larger proof for an already known
value without adding any new knowledge. Requiring proof trees themselves to
stabilize could make such a judgment iterate forever. The algorithm does not
promise to explore every possible proof or find a globally minimal one.

Proof trees are immutable shared nodes. Incorporating a subproof into another
proof or cloning a cached proof retains a reference to its existing nodes. This
preserves sharing as proofs are constructed; it does not search for and intern
independently constructed equal proofs. Proof size still counts occurrences in
the logical tree, so sharing does not change which proof is considered smaller.

The regression test
[`completed_chain_shares_subproofs_across_all_cached_results`](https://github.com/rust-lang/a-mir-formality/blob/main/crates/formality-core/src/judgment/memo.rs#L623)
checks this distinction: 256 cached proofs contain 32,896 logical node
occurrences but retain only 256 distinct allocated nodes.

Empty completed results are not memoized. A failed judgment also carries failure
diagnostics, and those are not represented in the memo table's output-to-proof
map. Reusing an empty map would lose those diagnostics. A repeated failed call
therefore executes its rules again. An active recursive call can still return
an empty approximation; that is cycle handling, not a cached failure.

For the implementation, see the module documentation in
[`runtime.rs`](https://github.com/rust-lang/a-mir-formality/blob/main/crates/formality-core/src/judgment/runtime.rs)
and
[`memo.rs`](https://github.com/rust-lang/a-mir-formality/blob/main/crates/formality-core/src/judgment/memo.rs).
