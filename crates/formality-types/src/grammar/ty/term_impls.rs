use crate::grammar::{Parameter};
use crate::FormalityLang;
use formality_core::{
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
