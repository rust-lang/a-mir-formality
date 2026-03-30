# Formality-core: left-recursion seed bias and `#[reject]`

## Context

Updating [dada-model](https://github.com/nikomatsakis/dada-model) to the latest formality-core commit (`1f47788e`) exposed a grammar ambiguity that the old parser silently resolved. Investigating it revealed an unintentional bias in the left-recursion machinery itself.

## The bug: right-associative seed bias

### Grammar (simplified from dada-model)

```rust
#[term]
pub enum Perm {
    #[grammar(given)]
    Given,

    #[grammar($v0 $v1)]
    Apply(Arc<Perm>, Arc<Perm>),  // perm composition, left-recursive
}
```

### Symptom

Parsing `given given given` as a `Perm` produces **only** the right-associative interpretation:

```
Apply(Given, Apply(Given, Given))
```

The left-associative `Apply(Apply(Given, Given), Given)` is never generated.

### Root cause

The left-recursion fixed-point iteration in `left_recursion.rs` stores only the **single longest** parse as the seed for reuse:

```rust
let best_value = all_values.iter().min_by_key(|s| s.text.len()).unwrap();
with_top!(|top| {
    top.value = Some(erase_type(best_value));
});
```

Tracing through `given given given`:

- **Round 0**: No seed → only `Given` succeeds (consuming `"given"`, leaving `" given given"`).
- **Round 1**: Seed = `Given`. `Apply`'s left-recursive `$v0` returns the seed. `$v1` is a fresh parse that independently produces `Given` and `Apply(Given, Given)`. Results: `Given`, `Apply(Given, Given)` (remainder `" given"`), `Apply(Given, Apply(Given, Given))` (remainder `""`).
- **Round 2**: Seed = `Apply(Given, Apply(Given, Given))` (the longest — consumed everything). `Apply`'s `$v0` returns this, but `$v1` has no text left → no new values → fixed point.

The intermediate result `Apply(Given, Given)` is never used as a seed, so the left-associative parse `Apply(Apply(Given, Given), Given)` is never produced.

### Cross-type ambiguity this causes

When a `Ty` type uses `Perm`:

```rust
#[term]
pub enum Ty {
    #[grammar($v0)]
    Named(TyId),

    #[grammar($v0 $v1)]
    ApplyPerm(Perm, Arc<Ty>),
}
```

Parsing `leaf x Data` produces two structurally different interpretations:

1. `ApplyPerm(Leaf, ApplyPerm(Id(x), Named(Data)))` — perm is just `leaf`
2. `ApplyPerm(Apply(Leaf, Id(x)), Named(Data))` — perm is `leaf x`

Both consume all input. In dada-model these are semantically equivalent (Apply is curried perm composition), but the parser doesn't know that, causing an ambiguity panic.

## Test results

Tests in `tests/parser-torture-tests/left_recursion_seed_bias.rs` reproduce both issues. These tests **pass** — they assert the current (buggy) behavior with FIXME comments noting the expected-correct values. When the seed bias is fixed, their assertions will need to be flipped:

### Seed bias tests (Perm grammar)

| Test | Input | Expected | Actual | Status |
|------|-------|----------|--------|--------|
| `two_leaves` | `leaf leaf` | 1 parse | 1 parse | ✓ |
| `three_leaves_both_associativities` | `leaf leaf leaf` | 2 parses (both assocs) | 1 parse (right-assoc only) | Bug confirmed |
| `four_leaves_associativity_bias` | `leaf leaf leaf leaf` | 5 parses (Catalan C₃) | <5 parses | Bug confirmed |
| `verify_seed_trace` | `leaf leaf leaf` | Both assocs present | Right-assoc present, left-assoc missing | Bug confirmed |

### Cross-type ambiguity tests (Ty using Perm)

| Test | Input | Result | Notes |
|------|-------|--------|-------|
| `ty_simple_perm_application` | `leaf Data` | 1 parse | No ambiguity — baseline |
| `ty_cross_type_ambiguity` | `leaf x Data` | 2 parses | Both interpretations produced — would panic with `term()` |
| `ty_deeper_cross_type_ambiguity` | `leaf x y Data` | 4 parses | Ambiguity compounds with more variables |

The earlier `left_recursion_seed.rs` test demonstrates a related issue: a shorter base-case parse that would compose with a left-recursive variant is shadowed by a longer base-case parse chosen as the seed.

## Analysis: three-layer architecture

Parsing disambiguation involves three orthogonal concerns, each needing its own mechanism:

### Layer 1: Precedence pruning (keep as-is)

The `#[precedence(N, left/right/none)]` machinery prunes the search space *during* parsing:

- **Left-recursion**: checks whether a seed's precedence is compatible with the current variant before reusing it
- **Right-recursion**: sets `min_precedence_level` to skip low-precedence variants in the right operand

This is **sound pruning** — it only eliminates parses that violate precedence/associativity rules. It uses only local information and reduces the search space efficiently. It does not need to change.

However, the pruning **interacts with the seed bias**: if the longest seed has the wrong precedence, the variant rejects it, but a shorter seed with the right precedence was never tried. Fixing the seed bias (layer 2) makes this interaction correct.

### Layer 2: Seed iteration (fix needed)

The fix: iterate over **all accumulated seeds** in the fixed-point loop, not just the longest. Each seed is independently checked for precedence compatibility. This produces all valid left-recursive compositions.

With the existing `left_associative` test (`a + b + c`):
- Round 0: no seed → base case produces `a`
- Round 1: seed = `a`, composes to `a+b`
- Round 2: seed = `a+b`, composes to `(a+b)+c`
- Right-associative `a+(b+c)` is blocked by `min_precedence_level` on right-recursion

This should still work correctly with all-seeds iteration — the precedence checks naturally reject wrong-associativity compositions regardless of which seed is tried.

### Layer 3: `#[reject]` attribute (new)

A new `#[reject(PATTERN)]` attribute for **semantic post-parse filtering**. This handles cases that precedence can't express — like "I don't want `Apply(..)` in this position."

## `#[reject]` design

### Syntax

Repeatable attribute on **variants** or **structs**. Multiple `#[reject]` attributes are OR'd — any match rejects the parse.

**Tuple fields** — positional, variant name is implicit:

```rust
#[term]
pub enum Ty {
    #[grammar($v0)]
    Named(TyId),

    #[grammar($v0 $v1)]
    #[reject(Perm::Apply(..), _)]          // reject when field 0 is a compound perm
    #[reject(Perm::Leaf, Ty::Named(..))]   // multiple rejects OK (OR'd)
    ApplyPerm(Perm, Arc<Ty>),
}
```

**Named fields** — use `name: pattern` syntax:

```rust
#[term($perm $ty)]
#[reject(perm: Perm::Apply(..), ty: _)]
pub struct ApplyPerm {
    perm: Perm,
    ty: Arc<Ty>,
}
```

The macro knows which form to expect because it has the variant/struct definition — if the fields are named, it expects `name: pattern` pairs; if tuple fields, positional patterns.

**Parsing strategy**: The attribute contents are parsed as a `Punctuated<TokenStream, Token![,]>` using `syn`. This splits on top-level commas only — commas nested inside `(..)`, `[..]`, or `{..}` are not treated as separators. This correctly handles patterns like `Perm::Apply(.., ..)`. Or-patterns at the field level must be parenthesized (e.g., `(Perm::Leaf | Perm::Given)`) since a bare `|` at the top level would be ambiguous.

**Trailing `..`**: A trailing `..` may be used to skip remaining fields (implying `_` for each). This works for both positional and named forms. `..` is only allowed at the end — leading or middle `..` is a compile error. If `..` is not present, all fields must be listed explicitly (using `_` for wildcards). Examples:
- `#[reject(Perm::Apply(..), ..)]` — check field 0 only, skip field 1
- `#[reject(perm: Perm::Apply(..), ..)]` — same, named form
- `#[reject(Perm::Apply(..))]` — **error**: missing field 1 (use `..` or `_`)

### Future extensions

- **Enum-level reject** — `#[reject(VariantName(...))]` on the enum itself, matching after construction and upcast. Not planned for the initial implementation.
- **Expression-based reject** — `#[reject(v => expr)]` for cases the pattern form can't express. Not planned for the initial implementation.

### Codegen insertion point

**Variant/struct-level**: injected into the tail of the variant parse, before `__p.ok(#construct)`. At this point all field variables (`v0`, `v1`, etc.) are in scope from the nested `each_nonterminal` closures.

Each field position in the reject pattern is processed independently:

- `_` or a bare binding (`x`) → **skip** (no constraint on this field)
- anything else → emit `matches!(DowncastFrom::downcast_from(&vN), Some(PATTERN))`

Non-skipped fields are AND'd. The `DowncastFrom` trait (see `formality_core::cast::DowncastFrom`) handles wrapper types transparently. The macro wraps the user's pattern in `Some(...)` and uses `DowncastFrom::downcast_from(&field)`, which returns `Option<T>`. Rust infers `T` from the pattern. This works uniformly whether the field is `Perm`, `Arc<Perm>`, `Box<Perm>`, etc. The user never thinks about wrappers.

**Verified**: type inference resolves correctly without turbofish in both cases. For `v0: Perm`, the chain is `Perm: DowncastTo<Perm>` (generated identity impl) → `Perm: DowncastFrom<Perm>` (blanket). For `v1: Arc<Ty>`, the macro generates `impl DowncastFrom<Arc<Self>> for Self` directly. In both cases, `matches!(DowncastFrom::downcast_from(&field), Some(Pattern::Variant(..)))` compiles and Rust infers the target type from the pattern.

Rejection is **silent** — it produces an empty success set, not an error. This avoids polluting error diagnostics with "almost parsed but rejected" messages.

```rust
// Generated code for ApplyPerm(Perm, Arc<Ty>) with:
//   #[reject(Perm::Apply(..), _)]
//   Field 0: Perm::Apply(..) → emit check
//   Field 1: _ → skip
let __rejected = matches!(DowncastFrom::downcast_from(&v0), Some(Perm::Apply(..)));
if __rejected {
    Ok(Default::default())
} else {
    __p.ok(Ty::ApplyPerm(v0.clone(), v1.clone()))
}
```

Within a single `#[reject]`, non-skipped fields are AND'd. Multiple `#[reject]` attributes are OR'd:
```rust
// #[reject(Perm::Apply(..), _)]           — reject compound perms
// #[reject(Perm::Leaf, Ty::Named(..))]    — reject leaf perm applied to bare named type
let __rejected =
    matches!(DowncastFrom::downcast_from(&v0), Some(Perm::Apply(..)))
    || (matches!(DowncastFrom::downcast_from(&v0), Some(Perm::Leaf))
        && matches!(DowncastFrom::downcast_from(&v1), Some(Ty::Named(..))));
if __rejected {
    Ok(Default::default())
} else {
    __p.ok(Ty::ApplyPerm(v0.clone(), v1.clone()))
}
```

For **named fields**, the same logic applies but field names are used instead of positional `vN`:

```rust
// #[reject(perm: Perm::Apply(..), ty: _)] on struct ApplyPerm { perm: Perm, ty: Arc<Ty> }:
let __rejected = matches!(DowncastFrom::downcast_from(&perm), Some(Perm::Apply(..)));
if __rejected {
    Ok(Default::default())
} else {
    __p.ok(ApplyPerm { perm: perm.clone(), ty: ty.clone() })
}
```

### Semantics

- Each `#[reject]` is a filter: if the pattern matches, the parse path is silently dropped (empty success set, not an error)
- Multiple `#[reject]` attributes are OR'd: any match → reject
- Each field's sub-pattern is a Rust pattern used inside `matches!(..., Some(PATTERN))` — the compiler validates it
- Rejection happens after all fields are parsed, before the value is constructed
- **Interaction with left-recursion**: rejected values never enter `self.successes`, so they never become seeds in the fixed-point loop. The transitivity implications depend on *which type* the `#[reject]` is placed on:
  - **Same-type reject** (e.g., `#[reject(...)]` on `Perm::Apply`): rejection is transitive within `Perm`'s left-recursion. A rejected `Apply(Leaf, Leaf)` never becomes a seed, so `Apply(Apply(Leaf, Leaf), Leaf)` is never explored. This is the desired behavior — a rejected value shouldn't compose further within the same type.
  - **Cross-type reject** (e.g., `#[reject(Perm::Apply(..), _)]` on `Ty::ApplyPerm`): this only affects `Ty`'s success set. `Perm::Apply(Leaf, Leaf)` is still a valid `Perm` parse and will still be a seed for `Perm`'s own left-recursion. The rejection only prevents that `Perm` value from being *used* in this particular `Ty` variant. This is also the desired behavior — the consumer restricts what it accepts without affecting the producer's parse space.

### Design rationale: why `#[reject]` and not grammar restructuring

Formality's `#[term]` macro unifies type definitions and grammar — the grammar *is* the type. This means restructuring a grammar to avoid parse ambiguity also restructures the types, which changes the semantic domain. That's undesirable.

For example, to avoid the cross-type ambiguity without `#[reject]`, you'd have to split `Perm` into `LeafPerm` and `CompoundPerm`, then restrict `Ty::ApplyPerm` to accept only `LeafPerm`. But `Perm` is `Apply(Arc<Perm>, Arc<Perm>)` because that's the right semantic representation — perm composition is a recursive operation. Splitting it pollutes the type structure with parser concerns.

`#[reject]` decouples parse-site disambiguation from type structure. The type (`Perm`) is defined to match the semantic domain. The consumer (`Ty::ApplyPerm`) decides what it accepts at the parse level without forcing the consumed type to restructure.

**When to use which mechanism:**

- **`#[precedence]`** for intra-type disambiguation — when a single recursive type produces multiple associativities or precedence orderings (e.g., `1 + 2 * 3`). This is the common case for expression grammars.
- **`#[reject]`** for cross-type disambiguation — when one type consumes another and the boundary between them is ambiguous (e.g., `Ty` consuming `Perm` doesn't know how much input `Perm` should take). Precedence can't help here because the ambiguity spans two different types.

## Implementation plan

This plan follows TDD discipline: every production code change is motivated by a failing test. Each step below is structured as Red→Green→Refactor cycles.

There are two independent bugs: the **seed bias** (intra-type, missing parses) and the **cross-type ambiguity** (inter-type, too many parses). The cross-type ambiguity already exists — the `ty_cross_type_ambiguity` and `ty_deeper_cross_type_ambiguity` tests demonstrate it today. `#[reject]` is not speculative; it's needed to resolve a known, reproduced problem. We build it first because it's self-contained and will also be needed when the seed bias fix surfaces additional ambiguities.

### Step 1: Build `#[reject]` (Red→Green)

The cross-type ambiguity tests already exist and demonstrate the problem. We write failing tests that assert the *desired* (disambiguated) behavior, then build `#[reject]` to make them pass.

#### 1a. Write failing tests (Red)

Add tests in `tests/parser-torture-tests/` that use `#[reject]` on a grammar and assert the disambiguated parse result. These tests won't compile yet (the attribute doesn't exist), which counts as Red.

Start with the minimal case: a `Ty::ApplyPerm` variant with `#[reject(Perm::Apply(..), _)]` that eliminates compound perms, asserting `leaf x Data` produces exactly 1 parse. Then add tests for:

- Single `#[reject]` on a variant with tuple fields
- Multiple `#[reject]` attributes (OR semantics)
- Named fields (`name: pattern` syntax)
- Trailing `..` to skip remaining fields
- Nested patterns (e.g., `Perm::Apply(Perm::Leaf, ..)`)
- Arc-wrapped fields (DowncastFrom interaction)
- Wildcard and bare-binding skip behavior
- Rejection + left-recursion interaction (rejected values don't become seeds within the same type)
- Cross-type reject doesn't affect the producer's seed space

Write each test, confirm it fails, then proceed to 1b.

#### 1b. Implement `#[reject]` (Green, per test)

For each failing test from 1a, implement the minimum machinery to make it pass:

- **First test**: requires basic macro parsing (`attrs.rs`) + codegen (`parse.rs`) for a single `#[reject]` on a variant with tuple fields. This is the minimal vertical slice.
- **Subsequent tests**: extend to named fields, multiple `#[reject]` (OR semantics), nested patterns, Arc-wrapped fields, etc.

**Macro parsing** (`attrs.rs`): parse `#[reject(...)]` attributes, splitting the contents into per-field patterns (positional for tuple fields, `name: pattern` for named fields). Each field's sub-pattern is stored as a token stream for later emission into `matches!`.

**Codegen** (`parse.rs`): for variant/struct level, inject `matches!` checks into the tail before `__p.ok(#construct)`. Add `use formality_core::cast::DowncastFrom;` to the `gen impl` block.

#### 1c. Verify existing tests unaffected (Refactor)

`#[reject]` is purely additive — existing tests should be unaffected. Run full `cargo test` to confirm.

### Step 2: Fix the seed bias (Red→Green)

#### 2a. Flip test expectations (Red)

Update the existing tests in `tests/parser-torture-tests/left_recursion_seed_bias.rs` to assert **correct** behavior:

- `three_leaves_both_associativities`: assert 2 parses (both associativities), not 1
- `four_leaves_associativity_bias`: assert 5 parses (Catalan C₃), not <5
- `verify_seed_trace`: assert both associativities present, not just right-assoc

Remove the FIXME comments. Run the tests — they should **fail** (Red).

Also update the earlier `left_recursion_seed.rs` test if it asserts buggy seed-shadowing behavior.

#### 2b. Fix seed iteration (Green)

Modify the left-recursion machinery to use **all accumulated values** as seeds, not just the longest. The goal: make the flipped tests pass.

**Mechanism**: The fixed-point loop in `enter` owns `all_values: Set<SuccessfulParse>`, a local variable on `enter`'s stack frame. Currently, each loop iteration stores a raw pointer to the single "best" *element inside* the set in `StackEntry.value`. The fix: change `StackEntry.value` to store a pointer to the **`all_values` set itself**.

Safety justification: `all_values` is a stack local in `enter`, so its address is stable for the lifetime of the function. The pointer is only dereferenced during `op()`, and `all_values` is not mutated during `op()` (`.extend()` happens after `op()` returns). This is actually a *simpler* invariant than the current one, which relies on element stability within the `BTreeSet`.

The pointer can be stored once before the loop begins — that is, after the first `op()` call returns and `all_values` is initialized, but before entering the `loop { }`. For Round 0 (the first `op()` call), `value` remains `None` and `observe` returns an empty set, preserving the current base-case behavior. The address of a stack local doesn't change, so re-storing each iteration (as the current code does) is unnecessary.

`observe` changes from returning `Option<SuccessfulParse>` (one seed) to `Set<SuccessfulParse>` (all accumulated seeds). It dereferences the pointer as `&Set<SuccessfulParse<'t, T>>`, clones the set, and returns it. When the set is empty (no seeds yet, i.e., the first round), it returns an empty set, which the caller treats as the base case (same as the current `None` path).

The precedence filtering stays where it is — in `enter`, right after calling `observe`. Currently it checks one seed against the current variant's precedence; with the fix, it filters the set:

```rust
let seeds = unsafe { entry.observe::<T>(text) };  // now returns Set
let valid_seeds: Set<_> = seeds.into_iter()
    .filter(|s| precedence_valid(s, current_precedence))
    .collect();
if valid_seeds.is_empty() {
    return ControlFlow::Break(Err(...));  // no valid seeds → base case
}
return ControlFlow::Break(Ok(valid_seeds));
```

This filters out seeds that can't appear as the left child of the current variant (e.g., a precedence-1 `a + b` can't be the left child of a precedence-2 `E * E`). Seeds that pass are all returned as a `Set<SuccessfulParse>` via `ControlFlow::Break(Ok(valid_seeds))`.

**How multi-seed fan-out propagates**: The returned set flows back through the call chain: `enter` → `T::parse` → `each_nonterminal`. The existing `each_nonterminal` implementation already iterates over all successes (`for success in successes { ... }`), forking the parser state for each one and running the continuation. This is how it handles ambiguous child parses generally — multiple seeds are just another source of multiple successes. No changes to `each_nonterminal` are needed.

Convergence detection is unchanged: the loop stops when `all_values` stops growing (no new values produced in a round). With all-seeds, the loop may take more rounds than before — shorter seeds can produce intermediate parses that weren't previously explored, which then compose further in subsequent rounds. Convergence is still guaranteed since the set of possible parses is finite (bounded by the Catalan number for the input length). For realistic formality grammars this is not a performance concern.

#### 2c. Verify existing precedence tests still pass (Refactor)

The existing `left_associative`, `right_associative`, and `none_associative` torture tests should still pass — precedence pruning handles them. If any fail, it indicates the multi-seed change interacts unexpectedly with precedence and needs adjustment. Fix before proceeding.

### Step 3: Resolve newly-surfaced ambiguities (Red→Green)

Run the full test suite (`cargo test`) after the seed bias fix. The fix will generate more parses than before, likely surfacing new ambiguity panics in formality-rust and possibly other crates.

For **each** new failure:
1. Examine the ambiguity — understand which parses are produced and which are unwanted
2. Add a **minimal focused test** in `tests/parser-torture-tests/` that reproduces the ambiguity in isolation, asserting the desired (disambiguated) result (Red)
3. Apply `#[reject]` or adjust `#[precedence]` to make it pass (Green)

Notes:
- `#[reject]` is already available from Step 1
- `Associativity::Both` (the default) is intentional — it means "this type has no precedence structure, accept anything." It does not need to change.
- If the number of new ambiguities is large or the fixes feel non-obvious, pause and check in with the human rather than pushing through mechanically

**Validation**: `cargo test` passes across all crates.

### Step 4: Update documentation

Update `book/src/formality_core/parse.md`:

- **New section: `#[reject]`** — syntax, variant-level vs enum-level, examples, when to use it. Frame the rationale: `#[term]` unifies types and grammars, so restructuring a grammar to avoid ambiguity means restructuring types, which pollutes the semantic domain. `#[reject]` lets parse-site disambiguation happen without forcing consumed types to restructure.
- **Update "Precedence and left-recursive grammars"** — explain the two disambiguation tools and when to use each: `#[precedence]` for intra-type ambiguity (expression associativity/precedence), `#[reject]` for cross-type ambiguity (boundary between two types is ambiguous). Mention the three-layer model.
- **Update "Ambiguity" section** — cross-reference both `#[precedence]` and `#[reject]` as disambiguation tools alongside the existing consume-all-input filtering
- **Reference the torture tests** — `left_recursion_seed_bias.rs` as examples

### Step 5: Validate against dada-model

Confirm that dada-model can use `#[reject]` to resolve the cross-type ambiguity from the motivating case.

## FAQ

**Q: Won't cloning the entire seed set in `observe` be slow for deeply ambiguous grammars?**

Performance is not a concern for now. Formality grammars are small and the parser is not on a hot path. If profiling later shows this matters, we can optimize (e.g., passing a reference instead of cloning, or a different channel for seed data).

**Q: Why does the rejection codegen use `Ok(Default::default())` instead of `Ok(set![])`?**

The `set!` macro is defined in `formality_core` and would need to be imported into the generated code's scope. `Default::default()` avoids that dependency — `BTreeSet` (which `Set` aliases) implements `Default` as the empty set, and the `Ok(...)` context provides enough type information for inference. It's less readable but avoids macro hygiene headaches in generated code.

**Q: Does `DowncastFrom::downcast_from(&field)` work for Arc-wrapped fields?**

The `#[term]` macro generates `impl DowncastFrom<Arc<Self>> for Self` (in `arc_cast`), so `DowncastFrom::downcast_from(&v1)` where `v1: Arc<Ty>` resolves to `Ty: DowncastFrom<Arc<Ty>>`. The identity case (`v0: Perm`) works via the generated `DowncastTo<Self> for Self`. However, there's some fishiness in the overall `Downcast`/`DowncastFrom`/`DowncastTo` impl landscape — some impls use `DowncastTo` (the "intended" trait to implement) and some directly impl `DowncastFrom`, with a blanket bridge between them. This could lead to coherence issues if we try to add new generic impls. **Follow-up work**: audit the downcast impls for consistency and potential overlap.

## Aside: `each_comma_nonterminal` / `each_delimited_nonterminal` collection genericity

**Fixed.** These methods now support generic `C: FromIterator<T>` instead of hardcoding `Vec<T>`.
