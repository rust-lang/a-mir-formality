//! Rewriting the lifetimes of a term. Codegen erases them all, as rustc
//! does: a lifetime never selects or distinguishes a monomorphization.

use crate::grammar::{AliasTy, Lt, Parameter, PredicateTy, RigidTy, Ty, Variable};
use formality_core::Upcast;
use std::sync::Arc;

/// Erase the lifetimes of `p`: every free lifetime becomes `'erased`, a
/// written `'static` included.
pub fn erase_lifetimes(p: &Parameter) -> Parameter {
    map_lifetimes(p, &mut erased)
}

fn erased(lt: &Lt) -> Lt {
    match lt {
        Lt::Variable(Variable::BoundVar(_)) => lt.clone(),
        _ => Lt::Erased,
    }
}

/// Rewrite the lifetimes of `p`: each lifetime `lt` becomes `f(lt)`.
fn map_lifetimes(p: &Parameter, f: &mut impl FnMut(&Lt) -> Lt) -> Parameter {
    match p {
        Parameter::Lt(lt) => f(lt).upcast(),
        Parameter::Ty(ty) => map_lifetimes_in_ty(ty, f).upcast(),
        Parameter::Const(_) => p.clone(),
    }
}

fn map_lifetimes_in_ty(ty: &Ty, f: &mut impl FnMut(&Lt) -> Lt) -> Ty {
    match ty {
        Ty::RigidTy(RigidTy { name, parameters }) => Ty::RigidTy(RigidTy {
            name: name.clone(),
            parameters: parameters.iter().map(|p| map_lifetimes(p, f)).collect(),
        }),
        Ty::AliasTy(AliasTy { name, parameters }) => Ty::AliasTy(AliasTy {
            name: name.clone(),
            parameters: parameters.iter().map(|p| map_lifetimes(p, f)).collect(),
        }),
        Ty::PredicateTy(PredicateTy::ForAll(binder)) => Ty::PredicateTy(PredicateTy::ForAll(
            binder.map(|ty| Arc::new(map_lifetimes_in_ty(&ty, f))),
        )),
        Ty::Variable(_) => ty.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::Binder;
    use crate::rust::term;
    use crate::rust::Term;

    /// `text`, a binder over one lifetime, instantiated with `'erased`,
    /// which the parser does not accept.
    fn erased_term<T: Term>(text: &str) -> T {
        let binder: Binder<T> = term(text);
        binder.instantiate_with(vec![Lt::Erased]).unwrap()
    }

    #[test]
    fn static_and_local_lifetimes_erase_alike() {
        let a: Parameter = term("&'static u32");
        let b: Parameter = erased_term("<'e> &'e u32");
        assert_eq!(erase_lifetimes(&a), b);
        assert_eq!(erase_lifetimes(&b), b);
    }

    #[test]
    fn bound_lifetimes_of_a_type_are_kept() {
        let ty: Parameter = term("for_all(<'a> &'a &'static u32)");
        let erased: Parameter = erased_term("<'e> for_all(<'a> &'a &'e u32)");
        assert_eq!(erase_lifetimes(&ty), erased);
    }
}
