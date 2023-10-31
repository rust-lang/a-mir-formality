use formality_core::{language::CoreKind, term::CoreTerm, visit::CoreVisit};

use crate::rust::Variable;

use super::{Lt, LtData, Parameter, ParameterKind, Ty};

use crate::FormalityLang as Rust;

impl CoreVisit<Rust> for LtData {
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

impl CoreTerm<Rust> for Ty {}

impl CoreTerm<Rust> for Lt {}

impl formality_core::language::HasKind<Rust> for Parameter {
    fn kind(&self) -> CoreKind<Rust> {
        match self {
            Parameter::Ty(_) => ParameterKind::Ty,
            Parameter::Lt(_) => ParameterKind::Lt,
            Parameter::Const(_) => ParameterKind::Const,
        }
    }
}
