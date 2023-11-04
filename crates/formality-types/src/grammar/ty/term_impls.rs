use crate::grammar::{Const, ConstData, Parameter, ValTree};
use crate::FormalityLang;
use formality_core::{
    fold::{CoreFold, SubstitutionFn},
    language::CoreKind,
};

use super::ParameterKind;

impl formality_core::language::HasKind<FormalityLang> for Parameter {
    fn kind(&self) -> CoreKind<FormalityLang> {
        match self {
            Parameter::Ty(_) => ParameterKind::Ty,
            Parameter::Lt(_) => ParameterKind::Lt,
            Parameter::Const(_) => ParameterKind::Const,
        }
    }
}

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
