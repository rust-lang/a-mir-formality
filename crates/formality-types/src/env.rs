use std::sync::Arc;

use crate::{
    grammar::{Fallible, Goal, KindedVarIndex, Parameter, ParameterKind, Ty, Universe},
    hook::Hook,
};

mod simple_sub;

pub struct Env {
    env: Arc<dyn EnvTrait>,
}

impl Env {
    pub fn instantiate_forall<T>(&mut self, binder: &Binder<T>) -> Vec<T> {}
    pub fn instantiate_exists<T>(&mut self, binder: &Binder<T>) -> Vec<T> {}
}

pub trait EnvTrait {
    fn instantiate_forall_vars(&self, var_ids: &[KindedVarIndex]) -> Vec<Parameter>;
    fn instantiate_exists_vars(&self, var_ids: &[KindedVarIndex]) -> Vec<Parameter>;
    fn equate(&self, a: &Parameter, b: &Parameter) -> Fallible<Vec<Goal>>;
    fn sub(&self, a: &Parameter, b: &Parameter) -> Fallible<Vec<Goal>>;
    fn outlives(&self, a: &Parameter, b: &Parameter) -> Fallible<Vec<Goal>>;
}
