use crate::grammar::{Const, ConstData, Lt, LtData, Parameter, Ty, TyData, ValTree};
use crate::rust::Variable;
use crate::FormalityLang;
use formality_core::{
    fold::{CoreFold, SubstitutionFn},
    language::CoreKind,
    term::CoreTerm,
    visit::CoreVisit,
    Upcast,
};

use super::ParameterKind;

impl CoreVisit<FormalityLang> for LtData {
    fn free_variables(&self) -> Vec<Variable> {
        match self {
            LtData::Variable(v) => {
                if v.is_free() {
                    vec![*v]
                } else {
                    vec![]
                }
            }
            LtData::Static => vec![],
        }
    }

    fn size(&self) -> usize {
        match self {
            LtData::Variable(v) => v.size(),
            LtData::Static => 1,
        }
    }

    fn assert_valid(&self) {
        match self {
            LtData::Variable(v) => v.assert_valid(),
            LtData::Static => (),
        }
    }
}

impl CoreTerm<FormalityLang> for Ty {}

impl CoreTerm<FormalityLang> for Lt {}

impl formality_core::language::HasKind<FormalityLang> for Parameter {
    fn kind(&self) -> CoreKind<FormalityLang> {
        match self {
            Parameter::Ty(_) => ParameterKind::Ty,
            Parameter::Lt(_) => ParameterKind::Lt,
            Parameter::Const(_) => ParameterKind::Const,
        }
    }
}

// ANCHOR: core_fold_ty
impl CoreFold<FormalityLang> for Ty {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, FormalityLang>) -> Self {
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
// ANCHOR_END: core_fold_ty

impl CoreFold<FormalityLang> for Const {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, FormalityLang>) -> Self {
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

impl CoreFold<FormalityLang> for ValTree {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, FormalityLang>) -> Self {
        self.clone()
    }
}

impl CoreFold<FormalityLang> for Lt {
    fn substitute(&self, substitution_fn: SubstitutionFn<'_, FormalityLang>) -> Self {
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

impl CoreVisit<FormalityLang> for Ty {
    fn free_variables(&self) -> Vec<Variable> {
        self.data().free_variables()
    }

    fn size(&self) -> usize {
        self.data().size()
    }

    fn assert_valid(&self) {
        self.data().assert_valid()
    }
}

impl CoreVisit<FormalityLang> for Lt {
    fn free_variables(&self) -> Vec<Variable> {
        self.data().free_variables()
    }

    fn size(&self) -> usize {
        self.data().size()
    }

    fn assert_valid(&self) {
        self.data().assert_valid()
    }
}
