use crate::grammar::Parameter;
use crate::FormalityLang;
use formality_core::language::CoreKind;
use formality_core::parse::CoreParseBinding;

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

impl CoreParseBinding<FormalityLang> for Parameter {
    fn parse_binding<'t>(
        scope: &formality_core::parse::Scope<FormalityLang>,
        text: &'t str,
    ) -> formality_core::parse::ParseResult<'t, formality_core::parse::Binding<FormalityLang>> {
        formality_core::parse::default_binding_parse(scope, text)
    }
}
