//! Handwritten parser impls.

use crate::parse::{self, expect_keyword, Parse};

use super::{AliasTy, Lt, LtData, Parameter, PredicateTy, RigidTy, ScalarId, Ty};

impl Parse for Ty {
    fn parse<'t>(scope: &crate::parse::Scope, text: &'t str) -> Option<(Self, &'t str)> {
        if let Some((scalar_ty, text)) = ScalarId::parse(scope, text) {
            Some((scalar_ty.into(), text))
        } else if let Some((p, text)) = parse_variable(scope, text) {
            match p {
                Parameter::Ty(ty) => Some((ty, text)),
                _ => None,
            }
        } else {
            parse::require_unambiguous(
                parse::try_parse(|| {
                    let (ty, text) = RigidTy::parse(scope, text)?;
                    Some((Ty::from(ty), text))
                })
                .into_iter()
                .chain(parse::try_parse(|| {
                    let (ty, text) = AliasTy::parse(scope, text)?;
                    Some((Ty::from(ty), text))
                }))
                .chain(parse::try_parse(|| {
                    let (ty, text) = PredicateTy::parse(scope, text)?;
                    Some((Ty::from(ty), text))
                })),
            )
        }
    }
}

impl Parse for Lt {
    fn parse<'t>(scope: &crate::parse::Scope, text: &'t str) -> Option<(Self, &'t str)> {
        parse::require_unambiguous(
            parse::try_parse(|| {
                let text = expect_keyword(text, "static")?;
                Some((Lt::from(LtData::Static), text))
            })
            .into_iter()
            .chain(parse::try_parse(|| {
                let (p, text) = parse_variable(scope, text)?;
                match p {
                    Parameter::Lt(lt) => Some((lt, text)),
                    _ => None,
                }
            })),
        )
    }
}

fn parse_variable<'t>(scope: &crate::parse::Scope, text: &'t str) -> Option<(Parameter, &'t str)> {
    let (id, text) = parse::identifier(text)?;
    let parameter = scope.lookup(&id)?;
    Some((parameter, text))
}
