# Intro

`a-mir-formality` is an early-stage experimental project maintained by the [Rust types team](https://www.rust-lang.org/governance/teams/compiler#types).
Its goal is to be a complete, authoritative formal model of the [Rust MIR](https://rustc-dev-guide.rust-lang.org/mir/index.html) and type system.
If successful, the intention is to bring this model into Rust as an RFC and develop it as an official part of the language definition.

## Workspace structure

The project is a Cargo workspace with three main crates, organized in layers:

- **`formality-macros`** — Procedural macros that generate boilerplate for formality terms. The `#[term]` macro auto-derives `Parse`, `Debug`, `Fold`, and `Visit` traits.
- **`formality-core`** — Language-independent foundation for formal semantics: variable binding, the judgment/proof system, fixed-point computation, and collections. Reusable for modeling any language, not just Rust.
- **`formality-rust`** — The Rust-specific model, containing three subsystems:
  - `grammar/` — AST definitions for types, traits, impls, functions, ADTs, and a MIR subset
  - `check/` — Semantic checking (entry point: `check_all_crates()`)
  - `prove/` — Trait solving and type normalization

The root `a-mir-formality` binary parses a Rust program file and runs `check_all_crates()`.

## Data flow

```
Program text
    → try_term()       (parse)
    → Program AST
    → check_all_crates()
    → per-crate checking
    → trait proving / normalization
    → ProofTree result
```

## Running tests

```bash
# Run the full test suite (what CI runs)
cargo test --all

# Run all targets including doc tests
cargo test --all-targets

# Run a single integration test by name
cargo test -p a-mir-formality -- test_name

# Run tests in a specific crate
cargo test -p formality-rust
cargo test -p formality-core
```

To see tracing output from a test, set the `RUST_LOG` environment variable:

```bash
RUST_LOG=debug cargo test -p a-mir-formality -- test_name
```

## Test macros

Integration tests use two snapshot-testing macros (powered by [`expect-test`](https://crates.io/crates/expect-test)):

- `assert_ok!([ ... ])` — asserts that a program type-checks successfully
- `assert_err!([ ... ] expect![[...]])` — asserts that a program fails with a specific error message
