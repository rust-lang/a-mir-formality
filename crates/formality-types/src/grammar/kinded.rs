use formality_core::Upcast;

use super::{BoundVar, Lt, LtData, ParameterKind, Ty, TyData};

/// Trait implemented by the various kinds of generic parameters.
/// Used in some of the fluent APIs for creating binders to select
/// the kind of generic variable that will be instantiated.
pub trait Kinded {
    fn instantiate() -> (Vec<BoundVar>, Self);
}

impl Kinded for Ty {
    fn instantiate() -> (Vec<BoundVar>, Self) {
        let bvar = BoundVar::fresh(ParameterKind::Ty);
        (vec![bvar], TyData::Variable(bvar.upcast()).upcast())
    }
}

impl Kinded for Lt {
    fn instantiate() -> (Vec<BoundVar>, Self) {
        let bvar = BoundVar::fresh(ParameterKind::Lt);
        (vec![bvar], LtData::Variable(bvar.upcast()).upcast())
    }
}

impl<A: Kinded, B: Kinded> Kinded for (A, B) {
    fn instantiate() -> (Vec<BoundVar>, Self) {
        let (a_names, a) = A::instantiate();
        let (b_names, b) = B::instantiate();
        ((a_names, b_names).upcast(), (a, b))
    }
}
