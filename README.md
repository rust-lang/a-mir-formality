# a-mir-formality

This repository is an early-stage experimental project that aims to be a complete, authoritative formal model of the [Rust MIR](https://rustc-dev-guide.rust-lang.org/mir/index.html).
Presuming these experiments bear fruit, the intention is to bring this model into Rust as an RFC
and develop it as an official part of the language definition.

## Quickstart guide

Like any Rust project:

* Clone
* `cargo test --all`

## Layers of formality

Formality is structured into several layers. These layers are meant to also map
fairly closely onto chalk and the eventual Rust trait solver implementation.
Ideally, one should be able to map back and forth between formality and the code
with ease.

* **formality-check**: Defines the top-level routines for checking Rust programs.
  `check_all_crates` is effectively the `main`, so it's a good place to start reading.
* **formality-core**: Defines logging macros.
* **formality-macros**: Defines procedural macros like `#[term]` as well as various derives.
  These are used to generate the boilerplate code for parsing, pretty printing, folding, etc.
* **formality-prove**: Defines the rules for proving goals (e.g., is this trait implemented?)
* **formality-rust:** This is the "Rust declarations" layer, defining Rust
  "top-level items" and their semantics. This includes crates, structs, traits,
  impls, but excludes function bodies.
* **formality-types:** This is the "types" layer, defining Rust types and
  functions for equating/relating them. The representation is meant to cover
  all Rust types, but is optimized for extracting their "essential properties".
