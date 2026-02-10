use expect_test::expect;
use formality_macros::test;
use crate::types::{
    grammar::{Wc, Wcs},
    rust::term,
};

use crate::prove::prove::{decls::Decls, prove::prove};

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Decls {
    Decls {
        trait_decls: vec![
            term("trait Eq<ty Self> where {PartialEq(Self)}"),
            term("trait PartialEq<ty Self> where {}"),
        ],
        ..Decls::empty()
    }
}

#[test]
fn eq_implies_partial_eq() {
    let assumptions: Wcs = Wcs::t();
    let goal: Wc = term("for<ty T> if {Eq(T)} PartialEq(T)");
    let constraints = prove(decls(), (), assumptions, goal);
    constraints.assert_ok(
    expect!["{Constraints { env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}

#[test]
fn not_partial_eq_implies_eq() {
    let goal: Wc = term("for<ty T> if {PartialEq(T)} Eq(T)");
    prove(decls(), (), (), goal)
    .assert_err(
    expect!["judgment had no applicable rules: `prove { goal: {for <ty> if {PartialEq(^ty0_0)} Eq(^ty0_0)}, assumptions: {}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, decls: decls(222, [trait Eq <ty> where {PartialEq(^ty0_0)}, trait PartialEq <ty> ], [], [], [], [], [], {}, {}, {}) }`"]);
}

#[test]
fn universals_not_eq() {
    let goal: Wc = term("for<ty T, ty U> if {Eq(T)} PartialEq(U)");
    prove(decls(), (), (), goal)
    .assert_err(
    expect![[r#"
        the rule "symmetric" at (prove_eq.rs) failed because
          cyclic proof attempt: `prove_eq { a: !ty_0, b: !ty_1, assumptions: {Eq(!ty_0)}, env: Env { variables: [!ty_0, !ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }`"#]]);
}
