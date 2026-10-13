//! Rewriting the lifetimes of a term. Codegen erases them all, as rustc
//! does: a lifetime never selects or distinguishes a monomorphization.

use crate::grammar::{AliasTy, Lt, Parameter, Predicate, RigidTy, TraitRef, Ty, Variable, Wc};
use formality_core::Upcast;

/// Erase the lifetimes of `p`: every free lifetime becomes `'erased`, a
/// written `'static` included.
pub fn erase_lifetimes(p: &Parameter) -> Parameter {
    map_lifetimes(p, &mut erased)
}

/// Erase the lifetimes of a trait bound, possibly under `for<..>` (whose
/// bound lifetimes are left alone).
pub fn erase_lifetimes_in_wc(wc: &Wc) -> Wc {
    match wc {
        Wc::Predicate(Predicate::IsImplemented(tr)) => {
            Predicate::is_implemented(map_lifetimes_in_trait_ref(tr, &mut erased)).upcast()
        }
        Wc::Predicate(Predicate::NotImplemented(tr)) => {
            Predicate::not_implemented(map_lifetimes_in_trait_ref(tr, &mut erased)).upcast()
        }
        Wc::ForAll(binder) => Wc::for_all(binder.map(|wc| erase_lifetimes_in_wc(&wc))),
        _ => wc.clone(),
    }
}

fn erased(lt: &Lt) -> Lt {
    match lt {
        Lt::Variable(Variable::BoundVar(_)) => lt.clone(),
        _ => Lt::Erased,
    }
}

/// Rewrite the lifetimes of a trait reference: each lifetime `lt` becomes
/// `f(lt)`.
pub(crate) fn map_lifetimes_in_trait_ref(tr: &TraitRef, f: &mut impl FnMut(&Lt) -> Lt) -> TraitRef {
    TraitRef {
        trait_id: tr.trait_id.clone(),
        parameters: tr.parameters.iter().map(|p| map_lifetimes(p, f)).collect(),
    }
}

/// Rewrite the lifetimes of `p`: each lifetime `lt` becomes `f(lt)`.
pub(crate) fn map_lifetimes(p: &Parameter, f: &mut impl FnMut(&Lt) -> Lt) -> Parameter {
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
        Ty::PredicateTy(_) | Ty::Variable(_) => ty.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rust::term;

    #[test]
    fn static_and_local_lifetimes_erase_alike() {
        let a: Parameter = term("&'static u32");
        let b: Parameter = term("&'erased u32");
        assert_eq!(erase_lifetimes(&a), b);
        assert_eq!(erase_lifetimes(&b), b);
    }

    #[test]
    fn bound_lifetimes_are_kept() {
        let wc: Wc = term("for<'a> Foo(&'a u32, &'static u32)");
        let erased: Wc = term("for<'a> Foo(&'a u32, &'erased u32)");
        assert_eq!(erase_lifetimes_in_wc(&wc), erased);
    }
}
