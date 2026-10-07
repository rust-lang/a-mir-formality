use super::Literal;
use crate::{
    grammar::{expr::IntegerValue, ScalarId},
    rust::FormalityLang as Rust,
};
use formality_core::{
    parse::{CoreParse, ParseError, ParseResult, Parser, Scope},
    Set,
};
use std::fmt::Debug;

impl Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}_{:?}", self.value, self.ty)
    }
}

const MAX_PLUS_ONE: u128 = 170141183460469231731687303715884105728; // i128::MAX + 1, or i128::MIN * (-1)

// Custom parsing for literals like `42_usize` || `42_i32`
// Let's both `42_u32` and `42 _ u32` be accepted
impl CoreParse<Rust> for Literal {
    fn parse<'t>(scope: &Scope<Rust>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "Literal", |avt| {
            let text0 = avt.text();
            let re = regex::Regex::new("-?").expect("to be right");
            let is_negative = avt.regex_str(&re, "an optional minus sign").is_ok();

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
                    IntegerValue::Signed(try_into_i128(number, is_negative, text0)?)
                } else {
                    IntegerValue::Unsigned(try_into_u128(number, is_negative, text0)?)
                };

                av.ok(Literal {
                    value: value.clone(),
                    ty,
                })
            })
        })
    }
}

fn try_into_i128(
    number: u128,
    is_negative: bool,
    text0: &str,
) -> Result<i128, Set<ParseError<'_>>> {
    // The value stored in `number` is the absolute value of the parsed number.  For example, in the
    // a-mir-formality program `-123 _ i128` the variable `number` contains the value 123, WITHOUT
    // the minus sign.
    //
    // Why is the special case for `(MAX_PLUS_ONE, true)` required?  Consider the following VALID
    // expression `-170141183460469231731687303715884105728 _ i128`. The parser will store the
    // absolute number value of this expression in `number`.  Without the specical case, we would
    // try to convert the `u128` into an `i128` using
    // `i128::try_from(170141183460469231731687303715884105728)`, which will yield an error, because
    // the number is to big and does not fit into an i128.  However, the whole expression is valid!
    // Therefore, we explicitly match this case, where the absolute value is equal to 2^127 + 1 AND
    // has a minus sign.  If that is the case, we know, that the parser has original read the
    // smallest possible `i128` number.
    match (number, is_negative) {
        (MAX_PLUS_ONE, true) => Ok(i128::MIN),
        (n, is_negative) => i128::try_from(n)
            .or_else(|_| Err(ParseError::at(text0, "invalid i128".to_string())))
            .map(|n| if is_negative { n * -1 } else { n }),
    }
}

fn try_into_u128(
    number: u128,
    is_negative: bool,
    text0: &str,
) -> Result<u128, Set<ParseError<'_>>> {
    if is_negative {
        return Err(ParseError::at(
            text0,
            "u128 can not be negative".to_string(),
        ));
    }
    Ok(number)
}
