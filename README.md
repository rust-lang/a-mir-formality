# a-mir-formality

This repository is an early-stage experimental project that aims to be a complete, authoritative formal model of the [Rust MIR](https://rustc-dev-guide.rust-lang.org/mir/index.html).
Presuming these experiments bear fruit, the intention is to bring this model into Rust as an RFC
and develop it as an official part of the language definition.

## Docs

Check out the [a-mir-formality Book](https://rust-lang.github.io/a-mir-formality/intro.html) for an overview and introduction to `a-mir-formality`.

## Quickstart guide

Like any Rust project:

* Clone
* `cargo test --all`

## Layers of formality

Formality is structured into several layers:

* **formality-macros**: Defines procedural macros like `#[term]` as well as various derives.
  These are used to generate the boilerplate code for parsing, pretty printing, folding, etc.
* **formality-core**: Language-independent foundation for formal semantics — variable binding,
  the judgment/proof system, fixed-point computation, and collections.
* **formality-rust**: The Rust-specific model. Contains `grammar/` (AST definitions),
  `check/` (semantic checking, with `check_all_crates` as the entry point),
  and `prove/` (trait solving and type normalization).
