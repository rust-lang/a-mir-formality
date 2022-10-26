use formality_types::{
    derive_links::Parameter,
    grammar::{Fallible, Goal},
};

use super::Env;

pub(super) fn outlives(_env: &Env, _a: &Parameter, _b: &Parameter) -> Fallible<(Env, Vec<Goal>)> {
    todo!()
}
