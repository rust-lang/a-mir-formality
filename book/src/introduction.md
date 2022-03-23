# A MIR Formality

A MIR Formality (or just Formality) is an early-stage experimental project that aims to be
a complete, authoritative formal model of the [Rust MIR][].
Presuming these experiments bear fruit,
the intention is to bring this model into Rust as an RFC
and develop it as an official part of the language definition.
We hope Formality will become the "primary source" for "what Rust means".

The goal of this document is to explain the project's structure as well
as give a sense of the overall development roadmap we have in mind.

<!-- Currently, Rust's semantics are defined ultimately by the compiler. -->
What we've done in the past is to have targeted models.
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

Formality also can be fuzzed and checked against rustc,
much as the [AWS S3 team does to check their code][].

We can extend rustc to generate formality declarations.
<!-- What does this mean? -->

For the trait solver, formality will map quite closely to its overall structure,
which allows us to experiment with new algorithms or structure and see their effects quickly.
As an initial example, I plan to prototype a new approach for associated type normalization.

<!--
## Previous approaches?
I had originally hoped that chalk could serve as a kind of "executable semantics" for Rust.
The idea was that the separation between "generating clauses" and "the solver"
would allow us to specify how Rust works on a high-enough level that the code is nicely generic.
Over time, though, I've become convinced that this approach won't scale.
There is a lot of "incidental complexity" that is created by integrating chalk into rustc,
as well as engineering for efficiency.
Ultimately, I don't see chalk being sufficiently malleable for our purposes.
-->

# Why PLT Redex?

MIR Formality is written in PLT Redex, a domain-specific language for working with semantics.
Redex is part of Racket,
a LISP-like language popular in programming language research.

It allows you to write very high-level structures, execute them, and see what happens.

I think it's sufficiently approachable that we can onboard contributors and grow the types team.
I'm not sure the same is true of other alternatives.

Redex also supports fuzzing.

[rust MIR]: https://rustc-dev-guide.rust-lang.org/mir/index.html
[AWS S3 team does to check their code]: https://www.amazon.science/publications/using-lightweight-formal-methods-to-validate-a-key-value-storage-node-in-amazon-s3
[rustc]: https://github.com/rust-lang/rust/compiler/
