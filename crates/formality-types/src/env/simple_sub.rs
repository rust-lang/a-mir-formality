use std::sync::Arc;

use crate::{
    grammar::{Parameter, ParameterKind, Ty, Universe},
    hook::Hook,
};

pub struct Environment {
    hook: Arc<dyn Hook>,
    universe: Universe,
    inference_data: Vec<InferenceVarData>,
}

struct InferenceVarData {
    ///
    kind: ParameterKind,

    universe: Universe,

    /// If `Some`, then this variable must be equal to the specific value
    /// given, and the other fields will all be empty.
    mapped_to: Option<Parameter>,

    /// Types that this variable is a subtype of.
    ///
    /// If this is a lifetime variable, this will be empty.
    subtype_of: Vec<Ty>,

    /// Types that this variable is a subtype of
    ///
    /// If this is a lifetime variable, this will be empty.
    supertype_of: Vec<Ty>,

    /// Types or lifetimes that this variable outlives.
    ///
    /// Values here will always have the same kind as this variable.
    outlives: Vec<Parameter>,
}

impl Environment {}
