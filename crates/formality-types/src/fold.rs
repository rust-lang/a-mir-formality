use crate::grammar::{Const, ConstData, Lt, LtData, Parameter, Ty, TyData, ValTree};

use crate::rust::FormalityLang as Rust;
use formality_core::fold::CoreFold;
use formality_core::fold::SubstitutionFn;
use formality_core::Upcast;

impl CoreFold<Rust> for Ty {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, Rust>) -> Self {
        match self.data() {
            TyData::RigidTy(v) => v.substitute(substitution_fn).upcast(),
            TyData::AliasTy(v) => v.substitute(substitution_fn).upcast(),
            TyData::PredicateTy(v) => v.substitute(substitution_fn).upcast(),
            TyData::Variable(v) => match substitution_fn(*v) {
                None => self.clone(),
                Some(Parameter::Ty(t)) => t,
                Some(param) => panic!("ill-kinded substitute: expected type, got {param:?}"),
            },
        }
    }
}

impl CoreFold<Rust> for Const {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, Rust>) -> Self {
        match self.data() {
            ConstData::Value(v, ty) => Self::valtree(
                v.substitute(substitution_fn),
                ty.substitute(substitution_fn),
            ),
            ConstData::Variable(v) => match substitution_fn(*v) {
                None => self.clone(),
                Some(Parameter::Const(c)) => c,
                Some(param) => panic!("ill-kinded substitute: expected const, got {param:?}"),
            },
        }
    }
}

impl CoreFold<Rust> for ValTree {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, Rust>) -> Self {
        self.clone()
    }
}

impl CoreFold<Rust> for Lt {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, Rust>) -> Self {
        match self.data() {
            LtData::Static => self.clone(),
            LtData::Variable(v) => match substitution_fn(*v) {
                None => self.clone(),
                Some(Parameter::Lt(t)) => t,
                Some(param) => panic!("ill-kinded substitute: expected lifetime, got {param:?}"),
            },
        }
    }
}
