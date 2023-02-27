use formality_macros::term;
use formality_types::{
    cast::Upcast,
    grammar::{Binder, InferenceVar, PlaceholderVar, Universe},
    term::Term,
};

#[term]
#[derive(Copy)]
pub struct Env {
    universe: Universe,
}

impl Env {
    pub fn instantiate_universally<T: Term>(&self, b: &Binder<T>) -> (T, Env) {
        let universe = self.universe.next();
        let result = b.instantiate(|kind, var_index| {
            PlaceholderVar {
                kind,
                universe,
                var_index,
            }
            .upcast()
        });
        (result, Env { universe })
    }

    pub fn instantiate_existentially<T: Term>(&self, b: &Binder<T>) -> (T, Env) {
        let universe = self.universe.next();
        let result = b.instantiate(|kind, var_index| {
            InferenceVar {
                kind,
                universe,
                var_index,
            }
            .upcast()
        });
        (result, Env { universe })
    }
}
