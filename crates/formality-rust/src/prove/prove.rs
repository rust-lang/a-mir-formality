//! This crate contains the trait proving + type inference logic.
//! It correpsonds loosely to the `InferenceContext` and trait solving (fulfillment context, etc)
//! in the Rust compiler.
//!
//! The base operations we export are:
//!
//! * [`prove`][] -- prove a set of where-clauses to be true
//! * [`prove_normalize`][] -- normalize a type one step (typically used in a recursive setup)

// Defines the language used by derive(term) and friends.
use crate::types::rust::FormalityLang;

mod db;
mod decls;
mod prove;

pub use decls::*;
pub use prove::combinators;
pub use prove::prove;
pub use prove::prove_normalize::prove_normalize;
pub use prove::Constraints;
pub use prove::{is_definitely_not_proveable, may_not_be_provable, negation_via_failure};
pub use prove::{Bias, Env};

#[cfg(test)]
mod test;

pub mod test_util;
