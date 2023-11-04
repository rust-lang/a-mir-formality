use crate::grammar::{Parameter, ValTree};
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

impl CoreFold<FormalityLang> for ValTree {
    fn substitute(&self, _substitution_fn: SubstitutionFn<'_, FormalityLang>) -> Self {
        self.clone()
    }
}
