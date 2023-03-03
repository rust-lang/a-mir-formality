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
        max_size: 22,
        trait_decls: vec![
            term("trait Eq<ty Self> where {is_implemented(PartialEq(Self))}"),
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
    let goal: Wc = term("for<ty T> if {is_implemented(Eq(T))} is_implemented(PartialEq(T))");
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
    let goal: Wc = term("for<ty T> if {is_implemented(PartialEq(T))} is_implemented(Eq(T))");
    let constraints = prove(program(), assumptions, goal);
    expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&constraints);
}

#[test]
fn placeholders_not_eq() {
    let assumptions: Wcs = Wcs::t();
    let goal: Wc = term("for<ty T, ty U> if {is_implemented(Eq(T))} is_implemented(PartialEq(U))");
    let constraints = prove(program(), assumptions, goal);
    expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&constraints);
}
