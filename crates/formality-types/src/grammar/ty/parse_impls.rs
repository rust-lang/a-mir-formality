//! Handwritten parser impls.

use crate::{
    cast::Upcast,
    grammar::{AdtId, AssociatedItemId, TraitId},
    parse::{self, expect_keyword, expect_str, Parse},
};

use super::{AliasTy, AssociatedTyName, Lt, LtData, Parameter, PredicateTy, RigidTy, ScalarId, Ty};

// For types, we invest some effort into parsing them decently because it makes
// writing tests so much more pleasant.
impl Parse for Ty {
    fn parse<'t>(scope: &crate::parse::Scope, text: &'t str) -> Option<(Self, &'t str)> {
        // Support writing `u8` etc and treat them as keywords
        if let Some((scalar_ty, text)) = ScalarId::parse(scope, text) {
            return Some((scalar_ty.upcast(), text));
        }

        // Support naming variables in scope and give that preference
        if let Some((p, text)) = parse_variable(scope, text) {
            return match p {
                Parameter::Ty(ty) => Some((ty, text)),
                _ => None,
            };
        }

        parse::require_unambiguous(
            std::iter::empty()
                .chain(parse::try_parse(|| parse_adt_ty(scope, text)))
                .chain(parse::try_parse(|| parse_assoc_ty(scope, text)))
                .chain(parse::try_parse(|| {
                    let (ty, text) = RigidTy::parse(scope, text)?;
                    Some((Ty::new(ty), text))
                }))
                .chain(parse::try_parse(|| {
                    let (ty, text) = AliasTy::parse(scope, text)?;
                    Some((Ty::new(ty), text))
                }))
                .chain(parse::try_parse(|| {
                    let (ty, text) = PredicateTy::parse(scope, text)?;
                    Some((Ty::new(ty), text))
                })),
        )
    }
}

#[tracing::instrument(level = "trace", ret)]
fn parse_adt_ty<'t>(scope: &crate::parse::Scope, text: &'t str) -> Option<(Ty, &'t str)> {
    // Treat plain identifiers as adt ids, with or without parameters.
    let (name, text) = AdtId::parse(scope, text)?;
    let (parameters, text) = parse_parameters(scope, text)?;
    Some((Ty::rigid(name, parameters), text))
}

#[tracing::instrument(level = "trace", ret)]
fn parse_assoc_ty<'t>(scope: &crate::parse::Scope, text: &'t str) -> Option<(Ty, &'t str)> {
    // Treat plain identifiers as adt ids, with or without parameters.
    let text = expect_str("<", text)?;
    let (ty0, text) = Ty::parse(scope, text)?;
    let text = expect_keyword("as", text)?;
    let (trait_id, text) = TraitId::parse(scope, text)?;
    let (trait_parameters1, text) = parse_parameters(scope, text)?;
    let text = expect_str(">", text)?;
    let text = expect_str("::", text)?;
    let (item_id, text) = AssociatedItemId::parse(scope, text)?;
    let (item_parameters, text) = parse_parameters(scope, text)?;

    let assoc_ty_id = AssociatedTyName { trait_id, item_id };
    let parameters: Vec<Parameter> = std::iter::once(ty0.upcast())
        .chain(trait_parameters1)
        .chain(item_parameters)
        .collect();
    Some((Ty::alias(assoc_ty_id, parameters), text))
}

#[tracing::instrument(level = "trace", ret)]
fn parse_parameters<'t>(
    scope: &crate::parse::Scope,
    text: &'t str,
) -> Option<(Vec<Parameter>, &'t str)> {
    let text = match expect_str("<", text) {
        None => return Some((vec![], text)),
        Some(text) => text,
    };
    let (parameters, text) = Parameter::parse_comma(scope, text, '>')?;
    let text = expect_str(">", text)?;
    Some((parameters, text))
}

impl Parse for Lt {
    fn parse<'t>(scope: &crate::parse::Scope, text: &'t str) -> Option<(Self, &'t str)> {
        parse::require_unambiguous(
            parse::try_parse(|| {
                let text = expect_keyword("static", text)?;
                Some((Lt::new(LtData::Static), text))
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
