# Layers of Formality

Formality is structured into several layers.
These layers are meant to also map fairly closely onto 
chalk and the eventual Rust trait solver implementation.
Ideally, one should be able to map back and forth between formality and the code with ease.
<!-- "the code": the rustc code? Chalk code? redex code? -->

## The types layer: `formality-ty`

Defines Rust types and functions for equating/relating them.
The representation is meant to cover all Rust types,
but is optimized for extracting their "essential properties".

Defines core logical predicates (`Implemented(T: Trait)`, etc) and solvers.
This layer does not define what they *mean* -- i.e., the conditions in which they are true.

## The "Rust declarations" layer: `formality-decl`

Defines Rust "top-level items" and their semantics.
This includes crates, structs, traits, impls, but excludes function bodies.

Semantics are defined by converting Rust items into predicates.
For example, `impl<T: Eq> Eq for Vec<T>` becomes a "program clause" (axiom)
like `forall<T> { Implemented(T: Eq) => HasImpl(Vec<T>: Eq) }`.
(The distinction between `HasImpl` and `Implemented` is covered below.)

This layer also defines the well-formedness checks for those items.
For example, `struct Foo { f1: T1 }` is well-formed if `T` is well-formed.

## The MIR type system layer: `formality-mir`

Defines the MIR and rules for its type checker.
This corresponds roughly to the MIR borrow checker + polonius.

<!-- Nick: and the type checker (and trait system) too, or is this just about lifetime checking? -->
<!-- Niko: all the things  -->
<!-- Niko: note that the MIR borrow checker includes a type checker -->

## The MIR operational semantics layer: `formality-mir-op`

Extends the above level with an operational semantics.
Basically equivalent to miri.
