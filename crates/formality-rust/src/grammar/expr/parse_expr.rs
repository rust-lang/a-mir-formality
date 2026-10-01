use super::Literal;
use crate::{
    grammar::{expr::IntegerValue, ScalarId},
    rust::FormalityLang as Rust,
};
use formality_core::parse::{CoreParse, ParseError, ParseResult, Parser, Scope};
use std::fmt::Debug;

impl Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}_{:?}", self.value, self.ty)
    }
}

// Custom parsing for literals like `42_usize` || `42_i32`
// Let's both `42_u32` and `42 _ u32` be accepted
impl CoreParse<Rust> for Literal {
    fn parse<'t>(scope: &Scope<Rust>, text: &'t str) -> ParseResult<'t, Self> {
        let MAX_PLUS_ONE: u128 = 170141183460469231731687303715884105728; // i128::MAX + 1, or i128::MIN * (-1)
        Parser::single_variant(scope, text, "Literal", |avt| {
            let text0 = avt.text();
            let re = regex::Regex::new("-?").expect("to be right");
            let has_sign = avt.regex_str(&re, "an optional minus sign").is_ok();

            let number: u128 = avt.number()?;
            avt.expect_char('_')?;

            avt.each_nonterminal(|ty: ScalarId, av| {
                if let ScalarId::Bool = ty {
                    return Err(ParseError::at(
                        av.text(),
                        "bool literal suffix are not allowed".to_string(),
                    ));
                }

                let value = if ty.is_signed() {
                    let n = if number == MAX_PLUS_ONE {
                        i128::MIN
                    } else {
                        i128::try_from(number)
                            .or_else(|_| Err(ParseError::at(text0, "invalid i128".to_string())))
                            .map(|n| if has_sign { n * -1 } else { n })?
                    };

                    IntegerValue::Signed(n)
                } else {
                    if has_sign {
                        return Err(ParseError::at(
                            text0,
                            "u128 can not be negative".to_string(),
                        ));
                    }
                    IntegerValue::Unsigned(number)
                };

                av.ok(Literal {
                    value: value.clone(),
                    ty,
                })
            })
        })
    }
}
