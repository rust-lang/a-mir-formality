use super::Literal;
use crate::{grammar::ScalarId, rust::FormalityLang as Rust};
use formality_core::parse::{CoreParse, ParseError, ParseResult, Parser, Scope};
use std::fmt::Debug;

impl Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}_{:?}", self.value, self.ty)
    }
}

// Custom parsing for literals like `42_usize` || `42_i32`
// Let's both `42_u32` and `42 _ u32` be accepted
impl CoreParse<Rust> for Literal {
    fn parse<'t>(scope: &Scope<Rust>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "Literal", |avt| {
            let value: usize = avt.number()?;
            avt.expect_char('_')?;
            avt.each_nonterminal(|ty: ScalarId, av| {
                if let ScalarId::Bool = ty {
                    return Err(ParseError::at(
                        av.text(),
                        "bool literal suffix are not allowed".to_string(),
                    ));
                }

                av.ok(Literal { value, ty })
            })
        })
    }
}
