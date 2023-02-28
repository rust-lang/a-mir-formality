use formality_macros::term;
use formality_types::grammar::{InferenceVar, Parameter};

#[term]
struct Env {}

#[term]
enum Op {
    Equals,
    Outlives,
}

#[term]
struct Incoming {
    lhs: InferenceVar,
    op: Op,
    rhs: Parameter,
}

#[term]
struct Outgoing {
    lhs: Parameter,
    op: Op,
    rhs: InferenceVar,
}
