//! Handwritten parser impls.

use crate::{
    cast::Upcast,
    grammar::{AdtId, AssociatedItemId, Bool, Const, RigidName, TraitId},
    parse::{self, expect_char, expect_keyword, reject_keyword, Parse, ParseError, ParseResult},
    seq,
};

use super::{AliasTy, AssociatedTyName, Lt, LtData, Parameter, PredicateTy, RigidTy, ScalarId, Ty};

// For types, we invest some effort into parsing them decently because it makes
// writing tests so much more pleasant.
impl Parse for Ty {
    fn parse<'t>(scope: &crate::parse::Scope, text0: &'t str) -> ParseResult<'t, Self> {
        // Support writing `u8` etc and treat them as keywords
        if let Ok((scalar_ty, text1)) = ScalarId::parse(scope, text0) {
            return Ok((scalar_ty.upcast(), text1));
        }

        // Support naming variables in scope and give that preference
        if let Ok((p, text1)) = parse_variable(scope, text0) {
            return match p {
                Parameter::Ty(ty) => Ok((ty, text1)),
                _ => Err(ParseError::at(
                    text0,
                    format!("expected type, found `{:?}`", p.kind()),
                )),
            };
        }

        parse::require_unambiguous(
            text0,
            vec![
                parse::try_parse(|| parse_adt_ty(scope, text0)),
                parse::try_parse(|| parse_assoc_ty(scope, text0)),
                parse::try_parse(|| parse_ref_ty(scope, text0)),
                parse::try_parse(|| parse_ref_mut_ty(scope, text0)),
                parse::try_parse(|| parse_tuple_ty(scope, text0)),
                parse::try_parse(|| {
                    let (ty, text) = RigidTy::parse(scope, text0)?;
                    Ok((Ty::new(ty), text))
                }),
                parse::try_parse(|| {
                    let (ty, text) = AliasTy::parse(scope, text0)?;
                    Ok((Ty::new(ty), text))
                }),
                parse::try_parse(|| {
                    let (ty, text) = PredicateTy::parse(scope, text0)?;
                    Ok((Ty::new(ty), text))
                }),
            ],
            "`Ty`",
        )
    }
}

#[tracing::instrument(level = "trace", ret)]
fn parse_adt_ty<'t>(scope: &crate::parse::Scope, text: &'t str) -> ParseResult<'t, Ty> {
    // Treat plain identifiers as adt ids, with or without parameters.
    let ((), text) = reject_keyword("static", text)?;
    let ((), text) = reject_keyword("const", text)?;
    let (name, text) = AdtId::parse(scope, text)?;
    let (parameters, text) = parse_parameters(scope, text)?;
    Ok((Ty::rigid(name, parameters), text))
}

#[tracing::instrument(level = "trace", ret)]
fn parse_ref_ty<'t>(scope: &crate::parse::Scope, text: &'t str) -> ParseResult<'t, Ty> {
    let ((), text) = expect_char('&', text)?;
    let (lt, text) = Lt::parse(scope, text)?;
    let (ty, text) = Ty::parse(scope, text)?;
    let name = crate::grammar::RigidName::Ref(crate::grammar::RefKind::Shared);
    Ok((
        RigidTy {
            name,
            parameters: seq![lt.upcast(), ty.upcast()],
        }
        .upcast(),
        text,
    ))
}

#[tracing::instrument(level = "trace", ret)]
fn parse_ref_mut_ty<'t>(scope: &crate::parse::Scope, text: &'t str) -> ParseResult<'t, Ty> {
    let ((), text) = expect_char('&', text)?;
    let ((), text) = expect_keyword("mut", text)?;
    let (lt, text) = Lt::parse(scope, text)?;
    let (ty, text) = Ty::parse(scope, text)?;
    let name = crate::grammar::RigidName::Ref(crate::grammar::RefKind::Mut);
    Ok((
        RigidTy {
            name,
            parameters: seq![lt.upcast(), ty.upcast()],
        }
        .upcast(),
        text,
    ))
}

#[tracing::instrument(level = "trace", ret)]
fn parse_tuple_ty<'t>(scope: &crate::parse::Scope, text: &'t str) -> ParseResult<'t, Ty> {
    let ((), text) = expect_char('(', text)?;
    let ((), text) = reject_keyword("rigid", text)?;
    let ((), text) = reject_keyword("alias", text)?;
    let ((), text) = reject_keyword("predicate", text)?;
    let (types, text) = Ty::parse_comma(scope, text, ')')?;
    let ((), text) = expect_char(')', text)?;
    let name = RigidName::Tuple(types.len());
    Ok((
        RigidTy {
            name,
            parameters: types.upcast(),
        }
        .upcast(),
        text,
    ))
}

#[tracing::instrument(level = "trace", ret)]
fn parse_assoc_ty<'t>(scope: &crate::parse::Scope, text: &'t str) -> ParseResult<'t, Ty> {
    // Treat plain identifiers as adt ids, with or without parameters.
    let ((), text) = expect_char('<', text)?;
    let (ty0, text) = Ty::parse(scope, text)?;
    let ((), text) = expect_keyword("as", text)?;
    let (trait_id, text) = TraitId::parse(scope, text)?;
    let (trait_parameters1, text) = parse_parameters(scope, text)?;
    let ((), text) = expect_char('>', text)?;
    let ((), text) = expect_char(':', text)?;
    let ((), text) = expect_char(':', text)?;
    let (item_id, text) = AssociatedItemId::parse(scope, text)?;
    let (item_parameters, text) = parse_parameters(scope, text)?;

    let assoc_ty_id = AssociatedTyName { trait_id, item_id };
    let parameters: Vec<Parameter> = std::iter::once(ty0.upcast())
        .chain(trait_parameters1)
        .chain(item_parameters)
        .collect();
    Ok((Ty::alias(assoc_ty_id, parameters), text))
}

#[tracing::instrument(level = "trace", ret)]
fn parse_parameters<'t>(
    scope: &crate::parse::Scope,
    text: &'t str,
) -> ParseResult<'t, Vec<Parameter>> {
    let text = match expect_char('<', text) {
        Err(_) => return Ok((vec![], text)),
        Ok(((), text)) => text,
    };
    let (parameters, text) = Parameter::parse_comma(scope, text, '>')?;
    let ((), text) = expect_char('>', text)?;
    Ok((parameters, text))
}

impl Parse for Lt {
    fn parse<'t>(scope: &crate::parse::Scope, text0: &'t str) -> ParseResult<'t, Self> {
        parse::require_unambiguous(
            text0,
            vec![
                parse::try_parse(|| {
                    let ((), text) = expect_keyword("static", text0)?;
                    Ok((Lt::new(LtData::Static), text))
                }),
                parse::try_parse(|| {
                    let (p, text1) = parse_variable(scope, text0)?;
                    match p {
                        Parameter::Lt(lt) => Ok((lt, text1)),
                        _ => Err(ParseError::at(
                            text0,
                            format!("expected lifetime, found `{:?}`", p.kind()),
                        )),
                    }
                }),
            ],
            "`Lt`",
        )
    }
}

#[tracing::instrument(level = "trace", ret)]
fn parse_variable<'t>(scope: &crate::parse::Scope, text0: &'t str) -> ParseResult<'t, Parameter> {
    let (id, text1) = parse::identifier(text0)?;
    match scope.lookup(&id) {
        Some(parameter) => Ok((parameter, text1)),
        None => Err(ParseError::at(text0, format!("unrecognized variable"))),
    }
}

// For consts, we invest some effort into parsing them decently because it makes
// writing tests so much more pleasant.
impl Parse for Const {
    fn parse<'t>(scope: &crate::parse::Scope, text: &'t str) -> ParseResult<'t, Self> {
        let ((), text) = expect_keyword("const", text)?;
        if let Ok((bool, text)) = Bool::parse(scope, text) {
            return Ok((bool.upcast(), text));
        }
        // Support naming variables in scope and give that preference
        if let Ok((p, text1)) = parse_variable(scope, text) {
            return match p {
                Parameter::Const(c) => Ok((c, text1)),
                _ => Err(ParseError::at(
                    text,
                    format!("expected type, found `{:?}`", p.kind()),
                )),
            };
        }
        parse::require_unambiguous(text, vec![], "`Const`")
    }
}
