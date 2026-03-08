use crate::grammar::Parameter;
use crate::FormalityLang;
use formality_core::language::CoreKind;
use formality_core::parse::{ActiveVariant, CoreParseBinding, ParseError, ParseResult, Scope};
use formality_core::variable::CoreBoundVar;
use formality_core::Set;

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
        scope: &Scope<FormalityLang>,
        text: &'t str,
    ) -> ParseResult<'t, formality_core::parse::Binding<FormalityLang>> {
        formality_core::parse::Parser::single_variant(scope, text, "Binding", |p| {
            let (kind, name) = if p.text().starts_with('\'') {
                // Lifetime bindings: `'a`
                p.expect_char('\'')?;
                let id = p.identifier_like_string()?;
                (ParameterKind::Lt, format!("'{}", id))
            } else if p.text().starts_with("const") {
                // Const bindings: `const C`
                p.expect_keyword("const")?;
                (ParameterKind::Const, p.identifier()?)
            } else {
                // Type bindings: `T`
                (ParameterKind::Ty, p.identifier()?)
            };
            let bound_var = CoreBoundVar::fresh(kind);
            p.ok(formality_core::parse::Binding { name, bound_var })
        })
    }

    fn parse_variable_name<'t>(
        p: &mut ActiveVariant<'_, 't, FormalityLang>,
    ) -> Result<String, Set<ParseError<'t>>> {
        // Try tick-prefixed identifier (e.g., `'a` for lifetime variables).
        // If text starts with `'`, parse `'` + identifier and return `"'name"`.
        // Otherwise fall back to regular identifier.
        if p.text().starts_with('\'') {
            p.expect_char('\'')?;
            let id = p.identifier_like_string()?;
            return Ok(format!("'{}", id));
        }

        p.identifier()
    }
}
