---
name: a-mir-formality-review-pr
description: Review a-mir-formality pull requests for Rust semantics, idiomatic judgments, and tests spanning formality, generated Rust, and MiniRust execution. Use for PR reviews and review-driven regression tests in this repository.
---

# Reviewing a-mir-formality pull requests

Review the implementation, its modeling choices, and the evidence from tests.
Explain what the change does and identify concrete counterexamples or missing
coverage. Keep semantic correctness and judgment style as separate review steps:
passing tests does not establish either one by itself.

## Establish the change being reviewed

Read the current PR description, diff, and relevant discussion. Verify the head
commit and base before testing; earlier reviews and local branches may be stale.
Preserve local work when checking out a PR. When recommending the next PR,
refresh the open queue and account for drafts, requested changes, and work the
user is doing elsewhere.

Trace the changed rule through its callers and shared helpers. A new operator
enum variant may also enable compound assignment; a translation change may
affect both associated types and ordinary parameters. Distinguish intended scope
from behavior enabled incidentally.

## Check judgment style explicitly

Read the companion skill at
`crates/formality-core/skills/a-mir-formality-idiomatic-judgment-fn/SKILL.md`
when reviewing judgments. Existing code is context, not an exemption from those
conventions. In particular:

- Match cases in conclusions and destructure judgment results directly.
- Express proof obligations as premises. Do not reduce proof search to a
  boolean or hide state threading in procedural control flow.
- Use generated term constructors and implicit borrowing/casting instead of
  explicit ownership bookkeeping. For example:

  ```rust,ignore
  (let expr = Expr::bin_op(lhs, BinOpKind::BitXor, rhs))
  ```

  Prefer that to constructing `Expr::BinOp` with cloned operands. `#[term]`
  constructors accept `impl Upcast<_>`; check the generated constructor or helper
  signature before concluding an explicit clone is necessary.
- Keep semantic cases in inference rules. Small, simple Rust computations and
  mechanical helpers can be appropriate; substantial `if`/`else` chains that
  implement proof search deserve scrutiny.

Run `cargo test --test judgment_lint` from the repository root. The test
calls `formality_lint::lint()` directly on Rust sources under `crates/`,
scanning only `judgment_fn!` bodies and reporting explicit clone calls. Review the flagged lines against the diff: existing findings are
cleanup candidates, not automatically regressions introduced by this PR.
The tool is deliberately narrow and does not replace the style review.

## Check semantics adversarially

Use the repository's pinned compiler as the Rust reference. Required language
feature gates belong in the input and its supported feature-gate machinery;
silently adding gates during translation does not establish end-to-end support.

Choose counterexamples around the changed behavior: quantifier and binder
scope, normalization, pending constraints, liveness, initialization, evaluation
order, or integer width and sign. Preserve all successful proofs: inference
rules are alternatives, not an ordered `if`/`else` chain. An unconditional move
rule does not by itself prove that a valid copy derivation cannot succeed.

For borrowing changes, make future reads and writes observable and check the
state propagated through branches, loops, and scope exit. For operators, inspect
precedence, associativity, operand evaluation order, and compound assignment.
Use side effects that distinguish competing evaluation orders.

For execution failures, distinguish the modeled outcome from a harness panic.
A MiniRust UB report is not evidence of a correctly modeled panic or abort.
Inspect the termination reason when that distinction is the regression.

## Exercise every applicable test layer

Inspect the current test harness before choosing assertions. Maximize coverage
of each test program through the available layers:

- Positive programs should check formality, compile the generated Rust with
  `.rustc_ok()`, and execute MiniRust with `.expect_output(...).ok()` where
  execution is supported. Supply a real `main` and observable results when
  needed. Do not add `.skip_execute()` merely to avoid providing a runnable
  program.
- Negative programs should assert both formality rejection and rustc rejection
  with `.rustc_err(...).err(...)`. Rejected programs cannot be executed; they
  do not need an explicit execution skip.
- A deliberately modeled difference from rustc should be documented and tested
  explicitly, rather than made to look like agreement. Identify known bugs by
  issue when retaining regression tests, and explain any ignored test.
- Structural parser tests are useful when behavior cannot distinguish parse
  trees. Prefer execution tests with distinct results when they can establish
  precedence or grouping; retain structural assertions for associative
  operators where different groupings yield the same result.

Be precise about evidence: `.rustc_ok()` checks generated-Rust compilation;
MiniRust output assertions are not differential execution against rustc. A
handwritten generated-Rust test can isolate lowering, but does not exercise the
whole formality-input translation path.

Run the affected suites and required repository checks. If a new regression
fails, minimize it and compare with the base branch when needed to distinguish
a PR regression from a pre-existing bug. Do not weaken a test to make it pass.
Report failures and intentional ignores separately from successful checks.

## Report and follow through

Lead with blocking findings, if any, and provide a reproducer, the expected and
actual behavior, and a precise code reference. Explain the algorithm and why
the failure follows from it. Separate correctness issues, modeling limitations,
style concerns, and optional coverage. If there are no blockers, say so while
stating the limits of the validation.

Draft review comments in neutral technical prose: describe the behavior and
the desired correction without imitating the user's voice or narrating the
investigation. A request for a draft does not authorize posting it.

When asked to add tests, preserve the distinction between exposing a bug and
fixing it. When asked to push, verify the PR's current repository, branch, and
head, then push only the intended commits. For a rebase, protect the remote
head with an explicit force-with-lease. Do not merge or post reviews merely
because the implementation looks ready.
