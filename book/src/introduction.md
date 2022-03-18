# A MIR Formality

MIR Formality (or just Formality) is an early-stage experimental project that aims to be
a complete, authoritative formal model of the [Rust MIR].
Presuming these experiments bear fruit,
the intention is to bring this model into Rust as an RFC
and develop it as an official part of the language definition.
We hope Formality will become the "primary source" for "what Rust means":

<!-- The goal of this document is to explain the high-level structure as well -->
<!-- as giving a sense for the overall development roadmap we have in mind. -->

Currently, Rust lacks SOMETHING.
Formality will give us a living model...

We imagine a future in which whenever a new language feature is designed,
one stage in the process will be formalizing that feature into Formality.
Currently, Since formality focuses on MIR, this will only make sense for "core language features"
like implied bounds or specialization.
Eventually I'd like to extend Formality to cover other areas
like name resolution and Rust surface syntax.
This will help to uncover interactions between features.

Formality can be fuzzed and checked against rustc,
much as the [AWS S3 team does to check their code](https://www.amazon.science/publications/using-lightweight-formal-methods-to-validate-a-key-value-storage-node-in-amazon-s3).
We can extend rustc to generate formality declarations.

For the trait solver, formality will map quite closely to its overall structure,
which allows us to experiment with new algorithms or structure and see their effects quickly.
As an initial example, I plan to prototype a new approach for associated type normalization.

<!--  -->
I had originally hoped that chalk could serve as a kind of "executable semantics" for Rust.
The idea was that the separation between "generating clauses" and "the solver"
would allow us to specify how Rust works on a high-enough level that the code is nicely generic.
Over time, though, I've become convinced that this approach won't scale.
There is a lot of "incidental complexity" that is created by integrating chalk into rustc,
as well as engineering for efficiency.
Ultimately, I don't see chalk being sufficiently malleable for our purposes.


[rust MIR]: https://rustc-dev-guide.rust-lang.org/mir/index.html
