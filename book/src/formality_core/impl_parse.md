# Implementing Parse by hand

The generic `#[term]` macro generates a simple parser, but sometimes you want more flexibility.
Here is the code that implements the parsing of Rust types:

```rust
{{#include ../../../crates/formality-types/src/grammar/ty/parse_impls.rs:ty_parse_impl}}
```
