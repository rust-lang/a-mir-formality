---
name: a-mir-formality-idiomatic-judgment-fn
description: Write, refactor, or review judgment_fn! definitions and their helpers in any project using formality-core. Express judgments as inference rules with conclusion patterns, implicit casting, proof premises, and explicit state threading.
---

# Idiomatic formality-core judgments

Make judgments read like type-system inference rules. These conventions apply to
any project built on `formality-core`, whether or not it models Rust. Let the DSL
handle ownership, conversions, proof search, and proof reporting.

The prefer/avoid examples below illustrate individual choices, not complete
programs. Names stand for the consuming project's terms and judgments; assume
the indicated casts exist. Existing code is useful evidence but does not override
these preferences.

## Match in the conclusion

Prefer case selection and destructuring in the conclusion. Give distinct cases
separate rules so the conclusion shows when each rule applies.

```rust,ignore
// Avoid: the conclusion hides which expression this rule handles.
(
    (if let Expr::Pair(left, right) = expr)
    (check(env, left) => ())
    (check(env, right) => ())
    --- ("pair")
    (check(env, expr) => ())
)

// Prefer: the case is visible in the conclusion; premises are its obligations.
(
    (check(env, left) => ())
    (check(env, right) => ())
    --- ("pair")
    (check(env, Expr::Pair(left, right)) => ())
)
```

An `if let` premise is appropriate only when the value cannot be destructured in
the conclusion or at the DSL construct that produces it.

## Destructure directly at the match site

Whenever a DSL construct matches a value, use the full useful pattern there.
Avoid assigning the value to an intermediate variable only to immediately
destructure that variable with `if let`. This applies to conclusion patterns,
judgment results after `=>`, and iteration or membership patterns.

```rust,ignore
// Avoid: `function_ty` exists only to be matched by the next premise.
(infer(env, function) => function_ty)
(if let Ty::Arrow(parameter_ty, result_ty) = function_ty)

// Prefer: destructure directly where the result is matched.
(infer(env, function) => Ty::Arrow(parameter_ty, result_ty))
```

Likewise, destructure collection elements directly in `(pattern in collection)`
or `for_all(pattern in collection)` when possible. Retain `if let` when matching
an ordinary computed value that has no earlier pattern-matching position, or when
the complete intermediate value is also needed.

## Match at the useful level of the type hierarchy

Leverage implicit downcasting to omit unnecessary outer wrappers. Prefer an enum
variant to an explicit typed downcast when a variant expresses the case.

```rust,ignore
// Avoid unnecessary wrappers in a conclusion argument:
OuterType::Inner(InnerType::Variant(x))
// Prefer, when the downcast exists:
InnerType::Variant(x)

// Avoid an explicit typed downcast when a constructor expresses the case:
x: Foo
// Prefer the directly relevant enum variant:
Ty::Foo(x)
```

Check the available casts when uncertain; not every wrapper is removable. For a
concrete example from a-mir-formality's Rust model, this rule directly matches an
`AliasTy` even though the judgment's input type is `Parameter`:

```rust,ignore
(
    (prove_alias_wf(decls, env, assumptions, name, parameters) => c)
    --- ("aliases")
    (prove_wf(decls, env, assumptions, AliasTy { name, parameters }) => c)
)
```

The example illustrates implicit downcasting; other projects need not define
`Parameter`, `AliasTy`, or this solver API.

## Let implicit borrowing and casting do the work

Judgment bindings are implicitly references and therefore copyable. Reusing a
binding does not move its referent. Avoid explicit clones, upcasts, and ownership
bookkeeping when implicit conversions suffice.

```rust,ignore
// Avoid: conversion and ownership machinery obscure the proof step.
(check(env.clone(), expr.clone().upcast()) => ())

// Prefer: the generated judgment accepts Upcast inputs, including references.
(check(env, expr) => ())
```

Use `(let ... = ...)` for auxiliary computations and `(if ...)` for guards.
Keep logical obligations visible rather than burying them in Rust blocks.

## Express proof search as premises

Avoid `.is_proven()` and similar boolean probes, or invoking judgments in ordinary
Rust control flow, unless absolutely necessary. Direct premises preserve proof
trees, failure explanations, and relevant alternative results.

```rust,ignore
// Avoid: reduces a proof to a boolean and loses its explanation.
(if check(env, expr).is_proven())
// Prefer: records the subproof and explains a failed obligation.
(check(env, expr) => ())

// Prefer: bind each successful judgment result for subsequent premises.
(infer(env, expr) => ty)

// Avoid: hides candidate selection and the proof behind iterator code.
(if candidates.iter().any(|candidate| check(env, candidate).is_proven()))
// Prefer: expose the candidate and the obligation separately.
(candidate in candidates)
(check(env, candidate) => ())
```

All applicable rules can contribute answers. Rule order is not an `if`/`else`
chain; an unconditional later rule is not a fallback.

## Use for_all and for_all/with for iteration

Use `for_all` and `for_all ... with(...)` whenever possible. Membership chooses a
candidate; universal iteration requires every element to pass and succeeds for an
empty collection.

```rust,ignore
// Avoid: hides universal checking and its individual proof failures.
(if items.iter().all(|item| check(env, item).is_proven()))
// Prefer:
(for_all(item in items)
    (check(env, item) => ()))

// Avoid: hides sequencing in an imperative block.
(let state = {
    let mut next = state.clone();
    for item in items {
        next = advance(&next, item);
    }
    next
})
// Prefer: make loop-carried state explicit.
(for_all(item in items) with(state)
    (let state = advance(state, item)))

// If advancing is itself a judgment, preserve its proof:
(for_all(item in items) with(state)
    (check_step(env, state, item) => state))
```

Thread updated state through outputs, usually shadowing names such as `state` or
`env`. Use distinct names when both versions are needed, such as branch states
that will be joined. Preserve substitutions and other accumulated constraints
using the consuming project's sequencing helpers.

Check branching requirements before replacing custom iteration. The macro loop
currently retains one successful accumulator outcome per iteration; a custom
combinator may instead explore multiple outcomes or apply substitutions to later
inputs. For example, a-mir-formality's solver `combinators::for_all` does both.

## Make helpers fit the rules

Accept `&Foo` or `impl Upcast<Foo>`. Prefer the latter when it avoids explicit
conversions at call sites. Avoid mutation of caller-owned state; local mutable
variables inside a helper are fine.

```rust,ignore
// Avoid: an owned-only interface forces conversion bookkeeping on callers.
fn inspect(term: Term) -> Info { /* ... */ }
(let info = inspect(expr.clone().upcast()))

// Prefer: accept compatible terms and convert inside the helper.
fn inspect(term: impl Upcast<Term>) -> Info {
    let term: Term = term.upcast();
    /* ... */
}
(let info = inspect(expr))

// Avoid: mutate caller-owned state.
fn add_fact(state: &mut State, fact: &Fact) { /* ... */ }
// Prefer: return updated state; local mutation can implement the computation.
fn add_fact(state: &State, fact: &Fact) -> State { /* ... */ }
(let state = add_fact(state, fact))
```

Extract substantial mechanical computations into helpers while keeping logical
obligations in the judgment. Proof-search helpers should preserve proof results
rather than reduce them to booleans.

## Use confirmation to reduce diagnostic noise

Use `!` after premises that establish whether a human would consider this rule a
candidate. If those premises fail, the rule was obviously inapplicable. Once they
succeed, failures in substantive proof obligations are worth reporting.

This often applies when case matching must move from the conclusion into an
`if let`, or when a high-level boolean split identifies the relevant case.

```rust,ignore
// Prefer: this computed classification cannot be matched in the conclusion.
// A different classification means the human would not consider this rule.
(if let Some(payload) = classify(input))!
(check_payload(env, payload) => ())

// Prefer: select the remote case, then retain failures explaining denied access.
(if mode.is_remote())!
(check_remote_access(env, request) => ())

// Avoid: confirming after the substantive obligation hides a relevant failure.
(if mode.is_remote())
(check_remote_access(env, request) => ())!
```

Confirmation is not logical cut: it does not prune other rules or alternative
proofs. It omits failures before confirmation from that rule's diagnostic output.
Use it judiciously: overuse or late placement can hide relevant information.
Do not add `!` mechanically to every guard or move it later merely to shorten an
error. The boundary is “obviously the wrong rule” versus “a plausible rule whose
obligations failed.”

## Assert only what ought to be impossible

Use assertions only for internal invariants whose violation indicates a bug in
the model or implementation. Ordinary rejection of an input is proof failure.

```rust,ignore
// Avoid: lacking permission is a legitimate reason for this proof to fail.
(assert permissions.contains(action))
// Prefer: report the unmet obligation without asserting an implementation bug.
(if permissions.contains(action))

// Appropriate only if construction guarantees this invariant:
(assert state.invariants_hold())
```

This distinction applies to header `assert(...)`, premise `(assert ...)`, and
assertions in helpers. Use a guard, failed subjudgment, or `(fail "...")` for
expected rejection. Give rules descriptive labels and useful `debug(...)` inputs.

## Preserve proof semantics

- Preserve quantification, assumption scope, binder scope exit, substitutions,
  and all relevant outputs when refactoring.
- `trivial(...)` intentionally short-circuits all normal rules. A premise-free
  rule still allows other rules to contribute answers.
- Recursive judgments use inductive fixed-point evaluation. Preserve monotonicity
  and convergence; an intermediate lack of proof is not logical negation.
- Consult the consuming project's version of `formality-core` for exact macro
  behavior and its own term definitions for casts. Do not assume a-mir-formality's
  repository layout or solver APIs are present.
- When reviewing, explain the convention and any semantic impact. When editing
  judgments, run relevant tests, including failure-output expectations when
  diagnostics change. Check that confirmation removes irrelevant noise while
  retaining the failures needed to understand an unsuccessful proof.
