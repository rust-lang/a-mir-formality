use std::sync::Arc;

use crate::{
    grammar::{
        Binder, InferenceVar, Parameter, ParameterKind, PlaceholderVar, Ty, Universe, VarIndex,
    },
    hook::Hook,
    term::Term,
};

mod simple_sub;

#[derive(Clone)]
pub struct Env {
    universe: Universe,
    inference_data: Vec<InferenceVarData>,
}

#[derive(Clone)]
struct InferenceVarData {
    /// Type, lifetime, etc
    kind: ParameterKind,

    /// Universe in which this variable was created.
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

impl Env {
    fn next_universe(&mut self) -> Universe {
        let u = self.universe;
        self.universe.index += 1;
        u
    }

    fn next_inference_variable(&mut self, kind: ParameterKind) -> InferenceVar {
        let index = self.inference_data.len();
        self.inference_data.push(InferenceVarData {
            kind,
            universe: self.universe,
            mapped_to: None,
            subtype_of: vec![],
            supertype_of: vec![],
            outlives: vec![],
        });
        InferenceVar { index }
    }

    pub fn instantiate_universally<T: Term>(&mut self, binder: &Binder<T>) -> T {
        let universe = self.next_universe();
        binder.instantiate(|kind, var_index| {
            PlaceholderVar {
                universe,
                var_index,
            }
            .into_parameter(kind)
        })
    }

    pub fn instantiate_existentially<T: Term>(&mut self, binder: &Binder<T>) -> T {
        binder
            .instantiate(|kind, _var_index| self.next_inference_variable(kind).into_parameter(kind))
    }
}
