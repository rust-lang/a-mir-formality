use formality_types::{
    cast::Upcast,
    grammar::{InferenceVar, Lt, Parameter, ParameterKind, Ty, VarIndex},
};

mod eq_partial_eq;
mod universes;

fn with_bindings<B: Bindings, R>(f: impl FnOnce(B) -> R) -> R {
    let b = B::instantiate(0);
    f(b)
}

trait Bindings {
    fn count() -> usize;
    fn instantiate(index: usize) -> Self;
}

impl Bindings for Ty {
    fn count() -> usize {
        1
    }

    fn instantiate(index: usize) -> Self {
        InferenceVar {
            kind: ParameterKind::Ty,
            var_index: VarIndex { index },
        }
        .upcast()
    }
}

impl Bindings for Lt {
    fn count() -> usize {
        1
    }

    fn instantiate(index: usize) -> Self {
        InferenceVar {
            kind: ParameterKind::Lt,
            var_index: VarIndex { index },
        }
        .upcast()
    }
}

impl<A, B> Bindings for (A, B)
where
    A: Bindings,
    B: Bindings,
{
    fn count() -> usize {
        A::count() + B::count()
    }

    fn instantiate(index: usize) -> Self {
        let a = A::instantiate(index);
        let b = B::instantiate(index + A::count());
        (a, b)
    }
}
