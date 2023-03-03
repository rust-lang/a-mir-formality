use expect_test::expect;
use formality_macros::test;
use formality_types::{
    grammar::{Wc, Wcs},
    parse::term,
};

use crate::{program::Program, prove::prove_wc_list};

/// Simple example program consisting only of two trait declarations.
const EQ_PARTIAL_EQ: &str = "
    program(
        [trait Eq<ty Self> where {is_implemented(PartialEq(Self))},
        trait PartialEq<ty Self> where {}],
        [],
        [],
        []
    )
";

#[test]
fn eq_implies_partial_eq() {
    let program: Program = term(EQ_PARTIAL_EQ);
    let assumptions: Wcs = Wcs::t();
    let goal: Wc = term("for<ty T> if {is_implemented(Eq(T))} is_implemented(PartialEq(T))");
    let constraints = prove_wc_list(program, assumptions, goal);
    expect![[r#"
        {
            <> Constraints { substitution: Substitution { map: {} } },
        }
    "#]]
    .assert_debug_eq(&constraints);
}

#[test]
fn not_partial_eq_implies_eq() {
    let program: Program = term(EQ_PARTIAL_EQ);
    let assumptions: Wcs = Wcs::t();
    let goal: Wc = term("for<ty T> if {is_implemented(PartialEq(T))} is_implemented(Eq(T))");
    let constraints = prove_wc_list(program, assumptions, goal);
    expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&constraints);
}

#[test]
fn placeholders_not_eq() {
    let program: Program = term(EQ_PARTIAL_EQ);
    let assumptions: Wcs = Wcs::t();
    let goal: Wc = term("for<ty T, ty U> if {is_implemented(Eq(T))} is_implemented(PartialEq(U))");
    let constraints = prove_wc_list(program, assumptions, goal);
    expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&constraints);
}
