# Implementing `Fold` and `Visit` by hand

The `#[term]` macro auto-generates impls of `Fold`, `Visit`, and `Parse`.
But sometimes you don't want to use the macro.
Sometimes you want to write your own code.
One common reason is for substitution of a variable.
For example, in the Rust model,
the code that replaces type variables with types from a substitution
is defined by a manual impl of `Fold`.
Because `Fold` and `Visit` are trait aliases, you need to implement the underlying
trait (`CoreFold`) by hand.
Here is the custom impl of fold for `Ty` from `formality_types`:

```rust
{{#include ../../../crates/formality-types/src/grammar/ty/term_impls.rs:core_fold_ty}}
```

That same module contains other examples, such as impls of `CoreVisit`.

## Derives

You can also manually derive `Visit` and `Fold` instead of using `#[term]`.