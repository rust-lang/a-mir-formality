use crate::cast::Upcast;

use super::{fresh_bound_var, BoundVar, Lt, ParameterKind, Ty};

/// Trait implemented by the various kinds of generic parameters.
/// Used in some of the fluent APIs for creating binders to select
/// the kind of generic variable that will be instantiated.
pub trait Kinded {
    fn instantiate() -> (Vec<BoundVar>, Self);
}

impl Kinded for Ty {
    fn instantiate() -> (Vec<BoundVar>, Self) {
        let bvar = fresh_bound_var(ParameterKind::Ty);
        (vec![bvar], bvar.ty())
    }
}

impl Kinded for Lt {
    fn instantiate() -> (Vec<BoundVar>, Self) {
        let bvar = fresh_bound_var(ParameterKind::Lt);
        (vec![bvar], bvar.lt())
    }
}

impl<A: Kinded, B: Kinded> Kinded for (A, B) {
    fn instantiate() -> (Vec<BoundVar>, Self) {
        let (a_names, a) = A::instantiate();
        let (b_names, b) = B::instantiate();
        ((a_names, b_names).upcast(), (a, b))
    }
}
