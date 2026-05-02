---
name: formality-best-practice
description: "[Critical] Best practices for writing judgment rules in formality-core. Always activate this skill when writing or modifying judgment_fn! code."
---

# Writing good judgment rules in formality-core

The judgment rules in this project will be converted into a language specification. They should read like inference rules on a whiteboard, not like Rust code crammed into a macro.

## Prefer judgment rules over regular functions

* If a function's name is not self-explanatory as a "pure data transformation," it should be a judgment rule.
* Regular Rust functions are fine for obvious transformations: type conversions, field lookups, simple constructors. Examples: `minirust_ty`, `scalar_minirust_ty`, `unit_value`.
* Anything with control flow, pattern matching on grammar types, or state threading belongs in a `judgment_fn!`.

## Avoid Rust-isms inside judgment rules

* **No IIFEs** — `(|| { ... })()` has no place in a rule. Split the logic into multiple rules or premises instead.
* **No `.clone()`** — use `Upcast` impls so the macro handles conversions automatically. If you find yourself writing `.clone()`, check whether an `Upcast` impl or `&self` method would eliminate it. (Exception: Vec indexing like `temps[i].clone()` and field access like `scope.ret_local.clone()` where the macro gives you a reference into a container.)
* **No `&` on arguments** — methods and free functions called from rules should accept `impl Upcast<T>` or `&self`, not require callers to take references.
* **No iterators or imperative loops** — use `for_all` to iterate. Thread state through `with(region, scope, global)`.
* **No raw MiniRust constructors** — wrap them in builder methods (`region.assign(target, load(source))`) and free functions (`constant`, `bool_constant`, `terminator_goto`).
* **No `Arc`, `GcCow`, or other smart pointers** in rules — hide them inside builder functions.

## Rule structure

* **Pattern match in conclusions** — `ExprData::Literal { value, ty }` in the conclusion, not `(if let ...)` guards. Use the upcast/downcast chain to match through nested types (e.g., `PlaceExprData::Var(id)` in a conclusion that expects `Expr`).
* **Construct result in conclusion** — `region.assign(target, constant(value, ty))` directly, not an intermediate variable.
* **Fallible calls go in premises** — the macro doesn't support `?` in conclusions. Move `addr_of(...)`, `tuple_value(...)`, etc. into `(let x = fallible_call(...)?)` premises.
* **Shadow threaded state** — when a method returns new state, rebind with the same name: `(let (x, global) = global.foo())`, not `(let (x, g2) = global.foo())`.

## Type conversions

* Return the most specific type from methods. Let callers adapt via `Upcast`.
* Use `formality_core::cast_impl!` to spell out transitive cast chains. Multiple invocations are fine and preferred — they prevent future surprises. Example:
  ```rust
  cast_impl!((PlaceExprData) <: (PlaceExpr) <: (ExprData));
  cast_impl!((PlaceExprData) <: (ExprData) <: (Arc<ExprData>));
  cast_impl!((PlaceExprData) <: (Arc<ExprData>) <: (Expr));
  ```
* Add `UpcastFrom` impls for foreign types that appear in judgment signatures (e.g., `UpcastFrom<lang::LocalName> for MrLocal`).

## Methods called from judgment rules

* Take `&self` and clone internally — the macro makes all bindings references.
* Accept `impl Upcast<T>` for parameters that may be references or wrapped types.
* Prefer infallible methods where possible (e.g., `assign` returns `Self`, not `Fallible<Self>`).

## Testing

* All existing tests must pass after each change — the refactoring is purely structural.
* Run `cargo test --all --workspace` after each phase.
* Write tests that exercise actual runtime behavior, not just compilation.
