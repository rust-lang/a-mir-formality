//! Handwritten parser impls.

use formality_core::parse::{ActiveVariant, CoreParse, ParseResult, Parser, Precedence, Scope};
use formality_core::seq;
use formality_core::Upcast;

use crate::grammar::{AdtId, AssociatedItemId, RefKind, RigidName, TraitId};

use super::{AliasTy, AssociatedTyName, Lt, Parameter, RigidTy, ScalarId, Ty};

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

                p.each_nonterminal(|name: AdtId, p| {
                    each_parse_parameters(p, |parameters, p| {
                        p.ok(RigidTy {
                            name: name.clone().upcast(),
                            parameters,
                        })
                    })
                })
            });

            // Parse `&`
            parser.parse_variant("Ref", Precedence::default(), |p| {
                p.expect_char('&')?;
                p.each_nonterminal(|lt: Lt, p| {
                    p.each_nonterminal(|ty: Ty, p| {
                        p.ok(RigidTy {
                            name: RigidName::Ref(RefKind::Shared),
                            parameters: seq![lt.clone().upcast(), ty.upcast()],
                        })
                    })
                })
            });

            parser.parse_variant("RefMut", Precedence::default(), |p| {
                p.expect_char('&')?;
                p.expect_keyword("mut")?;
                p.each_nonterminal(|lt: Lt, p| {
                    p.each_nonterminal(|ty: Ty, p| {
                        p.ok(RigidTy {
                            name: RigidName::Ref(RefKind::Mut),
                            parameters: seq![lt.clone().upcast(), ty.upcast()],
                        })
                    })
                })
            });

            parser.parse_variant("Tuple", Precedence::default(), |p| {
                p.expect_char('(')?;
                p.reject_custom_keywords(&["alias", "rigid", "predicate"])?;
                p.each_comma_nonterminal(|types: Vec<Ty>, p| {
                    p.expect_char(')')?;
                    let name = RigidName::Tuple(types.len());
                    p.ok(RigidTy {
                        name,
                        parameters: types.upcast(),
                    })
                })
            });
        })
    }
}
// ANCHOR_END: RigidTy_impl

impl CoreParse<Rust> for AliasTy {
    fn parse<'t>(scope: &Scope<Rust>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::multi_variant(scope, text, "AliasTy", |parser| {
            parser.parse_variant("associated type", Precedence::default(), |p| {
                p.expect_char('<')?;
                p.each_nonterminal(|ty0: Ty, p| {
                    p.expect_keyword("as")?;
                    p.each_nonterminal(|trait_id: TraitId, p| {
                        each_parse_parameters(p, |trait_parameters1, p| {
                            p.expect_char('>')?;
                            p.expect_char(':')?;
                            p.expect_char(':')?;
                            p.each_nonterminal(|item_id: AssociatedItemId, p| {
                                let trait_id = trait_id.clone();
                                let ty0 = ty0.clone();
                                let trait_parameters1 = trait_parameters1.clone();
                                each_parse_parameters(p, |item_parameters, p| {
                                    let name = AssociatedTyName {
                                        trait_id: trait_id.clone(),
                                        item_id: item_id.clone(),
                                        item_arity: item_parameters.len(),
                                    };
                                    let parameters: Vec<Parameter> =
                                        std::iter::once(ty0.clone().upcast())
                                            .chain(trait_parameters1.clone())
                                            .chain(item_parameters)
                                            .collect();
                                    p.ok(AliasTy {
                                        name: name.upcast(),
                                        parameters,
                                    })
                                })
                            })
                        })
                    })
                })
            });
        })
    }
}

fn each_parse_parameters<'s, 't, R: std::fmt::Debug + Clone + Eq + 'static>(
    p: &mut ActiveVariant<'s, 't, Rust>,
    op: impl Fn(Vec<Parameter>, &mut ActiveVariant<'s, 't, Rust>) -> ParseResult<'t, R>,
) -> ParseResult<'t, R> {
    if p.expect_char('<').is_err() {
        return op(vec![], p);
    }
    p.each_comma_nonterminal(|parameters: Vec<Parameter>, p| {
        p.expect_char('>')?;
        op(parameters, p)
    })
}
