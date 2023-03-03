use expect_test::expect;
use formality_macros::test;
use formality_types::{
    grammar::{Wc, Wcs},
    parse::term,
};

use crate::{program::Program, prove::prove_wc_list};

/// Simple example program consisting only of two trait declarations.
const PROGRAM: &str = "
    program(
        [
            trait Debug<ty Self> where {},
        ],
        [
            impl<ty T> Debug(Vec<T>) where {is_implemented(Debug(T))},
            impl<> Debug(u32) where {},
        ],
        [],
        []
    )
";

#[test]
fn vec_u32_debug() {
    let program: Program = term(PROGRAM);
    let assumptions: Wcs = Wcs::t();
    let goal: Wc = term("is_implemented(Debug(Vec<u32>))");
    let constraints = prove_wc_list(program, assumptions, goal);
    expect![[r#"
        {
            <> Constraints { known_true: true, substitution: Substitution { map: {} } },
        }
    "#]]
    .assert_debug_eq(&constraints);
}

#[test]
fn vec_vec_u32_debug() {
    let program: Program = term(PROGRAM);
    let assumptions: Wcs = Wcs::t();
    let goal: Wc = term("is_implemented(Debug(Vec<Vec<u32>>))");
    let constraints = prove_wc_list(program, assumptions, goal);
    expect![[r#"
        {
            <> Constraints { known_true: true, substitution: Substitution { map: {} } },
        }
    "#]]
    .assert_debug_eq(&constraints);
}
