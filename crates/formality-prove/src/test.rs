use expect_test::expect;
use formality_macros::test;
use formality_types::{
    grammar::{Wc, Wcs},
    parse::term,
};

use crate::{program::Program, prove::prove_wc_list};

#[test]
fn eq_partial_eq() {
    let program: Program = term(
        "
        program(
            [trait Eq<ty Self> where {is_implemented(PartialEq(Self))},
             trait PartialEq<ty Self> where {}],
            [],
            [],
            []
        )
        ",
    );

    let assumptions: Wcs = term("{}");

    let goal: Wc = term("for<ty T> if {is_implemented(Eq(T))} is_implemented(PartialEq(T))");

    let constraints = prove_wc_list(program, assumptions, goal);

    expect![].assert_debug_eq(&constraints);
}
