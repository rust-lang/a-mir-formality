# `formality_rust`: the Rust model

`formality_rust` is the Rust-specific layer of a-mir-formality. It is organized into three main subsystems:

* `grammar/` defines Rust-specific terms
* `check/` runs semantic checking
* `prove/` discharges logical obligations

## `grammar/`

The `grammar` module defines the Rust terms used by the model: crates, items, types, where-clauses, trait references, and the MIR-like expression language used in function bodies.

## `check/`

The main checking entry point is `check_all_crates`.

* inserts an empty `core` crate if the first crate is not `core`
* checks every prefix of the crate list
* treats the last crate in each prefix as the current crate

Within a crate, checking rejects duplicate item names, checks each crate item, and then runs coherence checking.

## `prove/`

The `prove` module answers Rust-specific goals such as where-clauses, equality, subtyping, and outlives. Checking code calls into `prove` whenever it needs to establish those facts.

## Pipeline

At a high level, checking a Rust program looks like this:

```text
Crates
  -> check_all_crates
  -> item checking / borrow checking
  -> prove obligations as needed
```