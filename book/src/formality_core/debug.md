# Customizing the debug

By default, the `#[term]` macro will generate a `Debug` impl that is guided by the `#[grammar]` attributes on your type (see the [parsing](./parse.md) section for more details). But sometimes you want to generate custom logic. You can include a `#[customize(debug)]` declaration to allow that. Most of the type, when you do this, you will also want to [customize parsing](./parse.md#customizing-the-parse), as the `RigidTy` does:

```rust
{{#include ../../../crates/formality-types/src/grammar/ty.rs:RigidTy_decl}}
```

Now you must simply implement `Debug` in the usual way. Here is the `RigidTy` declaration:


```rust
{{#include ../../../crates/formality-types/src/grammar/ty/debug_impls.rs:RigidTy_impl}}
```