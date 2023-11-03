//! Handwritten parser impls.

use formality_core::parse::{ActiveVariant, CoreParse, ParseError, ParseResult, Parser, Scope};
use formality_core::Upcast;
use formality_core::{seq, Set};

use crate::grammar::{AdtId, AssociatedItemId, Bool, Const, RigidName, Scalar, TraitId};

use super::{AliasTy, AssociatedTyName, Lt, LtData, Parameter, PredicateTy, RigidTy, ScalarId, Ty};

use crate::rust::FormalityLang as Rust;

// ANCHOR: ty_parse_impl
// For types, we invest some effort into parsing them decently because it makes
// writing tests so much more pleasant.
impl CoreParse<Rust> for Ty {
    fn parse<'t>(scope: &Scope<Rust>, text0: &'t str) -> ParseResult<'t, Self> {
        let mut parser = Parser::new(scope, text0, "Ty");
        parser.parse_variant_cast::<ScalarId>(1);
        parser.parse_variant("Variable", 1, |p| p.variable());
        parser.parse_variant("Adt", 0, |p| p.nonterminal_with(parse_adt_ty));
        parser.parse_variant("Assoc", 0, |p| p.nonterminal_with(parse_assoc_ty));
        parser.parse_variant("Ref", 0, |p| p.nonterminal_with(parse_ref_ty));
        parser.parse_variant("RefMut", 0, |p| p.nonterminal_with(parse_ref_mut_ty));
        parser.parse_variant("Tuple", 0, |p| p.nonterminal_with(parse_tuple_ty));
        parser.parse_variant_cast::<RigidTy>(0);
        parser.parse_variant_cast::<AliasTy>(0);
        parser.parse_variant_cast::<PredicateTy>(0);
        parser.finish()
    }
}
// ANCHOR_END: ty_parse_impl

#[tracing::instrument(level = "trace", ret)]
fn parse_adt_ty<'t>(scope: &Scope<Rust>, text: &'t str) -> ParseResult<'t, Ty> {
    // Treat plain identifiers as adt ids, with or without parameters.
    Parser::single_variant(scope, text, "AdtTy", |p| {
        let name: AdtId = p.nonterminal()?;
        let parameters: Vec<Parameter> = parse_parameters(p)?;
        Ok(Ty::rigid(name, parameters))
    })
}

#[tracing::instrument(level = "trace", ret)]
fn parse_ref_ty<'t>(scope: &Scope<Rust>, text: &'t str) -> ParseResult<'t, Ty> {
    Parser::single_variant(scope, text, "RefTy", |p| {
        p.expect_char('&')?;
        let lt: Lt = p.nonterminal()?;
        let ty: Ty = p.nonterminal()?;
        let name = crate::grammar::RigidName::Ref(crate::grammar::RefKind::Shared);
        Ok(RigidTy {
            name,
            parameters: seq![lt.upcast(), ty.upcast()],
        }
        .upcast())
    })
}

#[tracing::instrument(level = "trace", ret)]
fn parse_ref_mut_ty<'t>(scope: &Scope<Rust>, text: &'t str) -> ParseResult<'t, Ty> {
    Parser::single_variant(scope, text, "RefMutTy", |p| {
        p.expect_char('&')?;
        p.expect_keyword("mut")?;
        let lt: Lt = p.nonterminal()?;
        let ty: Ty = p.nonterminal()?;
        let name = crate::grammar::RigidName::Ref(crate::grammar::RefKind::Mut);
        Ok(RigidTy {
            name,
            parameters: seq![lt.upcast(), ty.upcast()],
        }
        .upcast())
    })
}

#[tracing::instrument(level = "trace", ret)]
fn parse_tuple_ty<'t>(scope: &Scope<Rust>, text: &'t str) -> ParseResult<'t, Ty> {
    Parser::single_variant(scope, text, "TupleTy", |p| {
        p.expect_char('(')?;
        p.reject_custom_keywords(&["alias", "rigid", "predicate"])?;
        let types: Vec<Ty> = p.comma_nonterminal()?;
        p.expect_char(')')?;
        let name = RigidName::Tuple(types.len());
        Ok(RigidTy {
            name,
            parameters: types.upcast(),
        }
        .upcast())
    })
}

#[tracing::instrument(level = "trace", ret)]
fn parse_assoc_ty<'t>(scope: &Scope<Rust>, text: &'t str) -> ParseResult<'t, Ty> {
    Parser::single_variant(scope, text, "AssocTy", |p| {
        p.expect_char('<')?;
        let ty0: Ty = p.nonterminal()?;
        let () = p.expect_keyword("as")?;
        let trait_id: TraitId = p.nonterminal()?;
        let trait_parameters1 = parse_parameters(p)?;
        p.expect_char('>')?;
        p.expect_char(':')?;
        p.expect_char(':')?;
        let item_id: AssociatedItemId = p.nonterminal()?;
        let item_parameters = parse_parameters(p)?;
        let assoc_ty_id = AssociatedTyName { trait_id, item_id };
        let parameters: Vec<Parameter> = std::iter::once(ty0.upcast())
            .chain(trait_parameters1)
            .chain(item_parameters)
            .collect();
        Ok(Ty::alias(assoc_ty_id, parameters))
    })
    // Treat plain identifiers as adt ids, with or without parameters.
}

fn parse_parameters<'t>(
    p: &mut ActiveVariant<'_, 't, Rust>,
) -> Result<Vec<Parameter>, Set<ParseError<'t>>> {
    if let Err(_) = p.expect_char('<') {
        return Ok(vec![]);
    }
    let parameters: Vec<Parameter> = p.comma_nonterminal()?;
    p.expect_char('>')?;
    Ok(parameters)
}

impl CoreParse<Rust> for Lt {
    fn parse<'t>(scope: &Scope<Rust>, text0: &'t str) -> ParseResult<'t, Self> {
        let mut parser = Parser::new(scope, text0, "Lt");
        parser.parse_variant("static", 0, |p| {
            p.expect_keyword("static")?;
            Ok(Lt::new(LtData::Static))
        });
        parser.parse_variant("variable", 0, |p| p.variable());
        parser.finish()
    }
}

// For consts, we invest some effort into parsing them decently because it makes
// writing tests so much more pleasant.
impl CoreParse<Rust> for Const {
    fn parse<'t>(scope: &Scope<Rust>, text: &'t str) -> ParseResult<'t, Self> {
        let mut parser: Parser<'_, '_, Const, Rust> = Parser::new(scope, text, "Ty");
        parser.parse_variant_cast::<Bool>(1);
        parser.parse_variant("Variable", 1, |p| p.variable());
        parser.parse_variant("Int", 0, |p| p.nonterminal_with(parse_int));
        parser.finish()
    }
}

#[tracing::instrument(level = "trace", ret)]
fn parse_int<'t>(scope: &Scope<Rust>, text: &'t str) -> ParseResult<'t, Const> {
    Parser::single_variant(scope, text, "Ty", |p| {
        let n: u128 = p.number()?;
        p.expect_char('_')?;
        let ty: Ty = p.nonterminal()?;
        Ok(Const::valtree(Scalar::new(n), ty))
    })
}
