use expect_test::expect;
use formality_macros::test;
use formality_types::{
    grammar::{Wc, Wcs},
    parse::term,
};

use crate::{program::Program, prove::prove};

/// Simple example program consisting only of two trait declarations.
fn program() -> Program {
    Program {
        max_size: Program::DEFAULT_MAX_SIZE,
        trait_decls: vec![
            term("trait Eq<ty Self> where {PartialEq(Self)}"),
            term("trait PartialEq<ty Self> where {}"),
        ],
        impl_decls: vec![],
        alias_eq_decls: vec![],
        alias_bound_decls: vec![],
    }
}

#[test]
fn eq_implies_partial_eq() {
    let assumptions: Wcs = Wcs::t();
    let goal: Wc = term("for<ty T> if {Eq(T)} PartialEq(T)");
    let constraints = prove(program(), assumptions, goal);
    expect![[r#"
        {
            <> Constraints { known_true: true, substitution: Substitution { map: {} } },
        }
    "#]]
    .assert_debug_eq(&constraints);
}

#[test]
fn not_partial_eq_implies_eq() {
    let assumptions: Wcs = Wcs::t();
    let goal: Wc = term("for<ty T> if {PartialEq(T)} Eq(T)");
    let constraints = prove(program(), assumptions, goal);
    expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&constraints);
}

#[test]
fn placeholders_not_eq() {
    let assumptions: Wcs = Wcs::t();
    let goal: Wc = term("for<ty T, ty U> if {Eq(T)} PartialEq(U)");
    let constraints = prove(program(), assumptions, goal);
    expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&constraints);
}
