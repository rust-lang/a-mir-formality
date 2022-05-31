---
sidebar_position: 1
---

# Introduction

**A MIR Formality** (or just Formality) is an early-stage experimental project that aims to be
a complete, authoritative formal model of the [Rust MIR][].
The project has two goals.
First, the models of Rust's MIR given by Formality's will serve as a complete model of _safe Rust_.
including all static checks and operational semantics.
This will provide a source of truth.

Presuming these experiments bear fruit,
the intention is to bring this model into Rust as an RFC
and develop it as an official part of the language definition.
We hope Formality will become the "primary source" for "what Rust means".

The goal of this document is to explain the project's structure as well
as give a sense of the overall development roadmap we have in mind.

# The problem

Currently, Rust's semantics are defined ultimately by the compiler.
Where more specific definitions were wanted, in the past we have created is to have targeted models.
But we're running up against the limit of what you can achieve that way.
Unsoundness arises in the "intersection" of different things,
and the system is too complex for us to keep in our heads.
On the other hand, writing Rust code is too slow and inflexible.
Writing a formal model helps us to scale.

Formality will give us a living, executable, and extensible model for Rust.
We imagine a future in which whenever a new language feature is designed,
one stage in the process will be formalizing that feature into Formality.
Currently, Formality focuses on MIR, which mean this will only make sense for "core language features"
like implied bounds or specialization.
Eventually we'd like to extend Formality to cover other areas
like name resolution and Rust surface syntax.
This will help to uncover interactions between features.

For the trait solver, Formality will map quite closely to its overall structure,
which allows us to experiment with new algorithms or structure and see their effects quickly.
Early prototyping includes approaches for such concepts as 
associated type normalization, perfect bounds, and subtyping.
These are not always features that users see directly.
Instead, Formality allows the Rust language 
to take advantage of type theory and other formal work on programming languages
in a way that can give a satisfying definition of the language,
and a practical tool for ensuring Rust has a solid foundation
(much as the [AWS S3 team does to check their code][]).

# Why PLT Redex?

Rust is a big language, with lots of features.
As a community of open source maintainers, programmers and researchers of all kinds,
we need a shared, well-known way of specifying the language.
We want to decide what Rust programs are:
how they are constructed (their syntax) and what they do (semantics).

MIR Formality is written in PLT Redex, a domain-specific language for working with semantics.
Redex is part of Racket, a LISP-like language popular in programming language research.
Redex programs are _executable models_.
It allows langauge designers to write very high-level structures, execute them, and see what happens.
PLT/Redex allows us to realize the dream of a One True Rust in a practical way,
checking `rustc` against a source of truth.

[rust MIR]: https://rustc-dev-guide.rust-lang.org/mir/index.html
[AWS S3 team does to check their code]: https://www.amazon.science/publications/using-lightweight-formal-methods-to-validate-a-key-value-storage-node-in-amazon-s3
[rustc]: https://github.com/rust-lang/rust/compiler/
