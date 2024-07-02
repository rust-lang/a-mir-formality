//! Handwritten parser impls.

use formality_core::parse::{
    ActiveVariant, CoreParse, ParseError, ParseResult, Parser, Precedence, Scope,
};
use formality_core::Upcast;
use formality_core::{seq, Set};

use crate::grammar::{
    AdtId, AssociatedItemId, Bool, ConstData, FnId, RefKind, RigidName, Scalar, TraitId,
};

use super::{AliasTy, AssociatedTyName, Lt, Parameter, ParameterKind, RigidTy, ScalarId, Ty};

use crate::rust::FormalityLang as Rust;

// ANCHOR: RigidTy_impl
// Implement custom parsing for rigid types.
impl CoreParse<Rust> for RigidTy {
    fn parse<'t>(scope: &Scope<Rust>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::multi_variant(scope, text, "RigidTy", |parser| {
            // Parse a `ScalarId` (and upcast it to `RigidTy`) with the highest
            // precedence. If someone writes `u8`, we always interpret it as a
            // scalar-id.
            parser.parse_variant_cast::<ScalarId>(Precedence::default());

            // Parse something like `Id<...>` as an ADT.
            parser.parse_variant("Adt", Precedence::default(), |p| {
                // Don't accept scalar-ids as Adt names.
                p.reject_nonterminal::<ScalarId>()?;

                let name: AdtId = p.nonterminal()?;
                let parameters: Vec<Parameter> = parse_parameters(p)?;
                Ok(RigidTy {
                    name: name.upcast(),
                    parameters,
                })
            });

            // Parse `&`
            parser.parse_variant("Ref", Precedence::default(), |p| {
                p.expect_char('&')?;
                let lt: Lt = p.nonterminal()?;
                let ty: Ty = p.nonterminal()?;
                Ok(RigidTy {
                    name: RigidName::Ref(RefKind::Shared),
                    parameters: seq![lt.upcast(), ty.upcast()],
                })
            });

            parser.parse_variant("RefMut", Precedence::default(), |p| {
                p.expect_char('&')?;
                p.expect_keyword("mut")?;
                let lt: Lt = p.nonterminal()?;
                let ty: Ty = p.nonterminal()?;
                Ok(RigidTy {
                    name: RigidName::Ref(RefKind::Mut),
                    parameters: seq![lt.upcast(), ty.upcast()],
                })
            });

            parser.parse_variant("Tuple", Precedence::default(), |p| {
                p.expect_char('(')?;
                p.reject_custom_keywords(&["alias", "rigid", "predicate"])?;
                let types: Vec<Ty> = p.comma_nonterminal()?;
                p.expect_char(')')?;
                let name = RigidName::Tuple(types.len());
                Ok(RigidTy {
                    name,
                    parameters: types.upcast(),
                })
            });

            parser.parse_variant("Fn", Precedence::default(), |p| {
                // parses 'fn name<params>' as fn-def
                // or 'fn(params) -> ty' as fn-ptr
                p.expect_keyword("fn")?;

                if p.expect_char('(').is_ok() {
                    p.reject_custom_keywords(&["alias", "rigid", "predicate"])?;
                    let mut types: Vec<Ty> = p.comma_nonterminal()?;
                    p.expect_char(')')?;
                    let name = RigidName::FnPtr(types.len());
                    if p.expect_char('-').is_ok() {
                        p.expect_char('>')?;
                        let ret = p.nonterminal()?;
                        types.push(ret);
                    } else {
                        types.push(Ty::unit());
                    }
                    Ok(RigidTy {
                        name,
                        parameters: types.upcast(),
                    })
                } else {
                    let name: FnId = p.nonterminal()?;
                    let parameters: Vec<Parameter> = parse_parameters(p)?;
                    Ok(RigidTy {
                        name: RigidName::FnDef(name),
                        parameters,
                    })
                }
            })
        })
    }
}
// ANCHOR_END: RigidTy_impl

impl CoreParse<Rust> for AliasTy {
    fn parse<'t>(scope: &Scope<Rust>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::multi_variant(scope, text, "AliasTy", |parser| {
            parser.parse_variant("associated type", Precedence::default(), |p| {
                p.expect_char('<')?;
                let ty0: Ty = p.nonterminal()?;
                p.expect_keyword("as")?;
                let trait_id: TraitId = p.nonterminal()?;
                let trait_parameters1 = parse_parameters(p)?;
                p.expect_char('>')?;
                p.expect_char(':')?;
                p.expect_char(':')?;
                let item_id: AssociatedItemId = p.nonterminal()?;
                let item_parameters = parse_parameters(p)?;
                let name = AssociatedTyName {
                    trait_id,
                    item_id,
                    item_arity: item_parameters.len(),
                };
                let parameters: Vec<Parameter> = std::iter::once(ty0.upcast())
                    .chain(trait_parameters1)
                    .chain(item_parameters)
                    .collect();
                Ok(AliasTy {
                    name: name.upcast(),
                    parameters,
                })
            });
        })
    }
}

fn parse_parameters<'t>(
    p: &mut ActiveVariant<'_, 't, Rust>,
) -> Result<Vec<Parameter>, Set<ParseError<'t>>> {
    if p.expect_char('<').is_err() {
        return Ok(vec![]);
    }
    let parameters: Vec<Parameter> = p.comma_nonterminal()?;
    p.expect_char('>')?;
    Ok(parameters)
}

// For consts, we invest some effort into parsing them decently because it makes
// writing tests so much more pleasant.
impl CoreParse<Rust> for ConstData {
    fn parse<'t>(scope: &Scope<Rust>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::multi_variant(scope, text, "ConstData", |parser| {
            parser.parse_variant("Variable", Precedence::default(), |p| {
                p.variable_of_kind(ParameterKind::Const)
            });

            parser.parse_variant_cast::<Bool>(Precedence::default());

            parser.parse_variant("Int", Precedence::default(), |p| {
                let n: u128 = p.number()?;
                p.expect_char('_')?;
                let ty: Ty = p.nonterminal()?;
                Ok(ConstData::Value(Scalar::new(n).upcast(), ty))
            });
        })
    }
}
