use expect_test::expect;
use formality_core::test;
use formality_types::{
    grammar::{Parameter, Relation, Wcs},
    rust::term,
};

use crate::{decls::Decls, prove::prove, Env};

fn decls() -> Decls {
    Decls {
        trait_decls: vec![term("trait Foo<ty Self> where {}")],
        impl_decls: vec![term("impl Foo(u32) where {}")],
        adt_decls: vec![term("adt X<ty T> where {Foo(T)} {}")],
        ..Decls::empty()
    }
}

#[test]
fn well_formed_adt() {
    let assumptions: Wcs = Wcs::t();
    let goal: Parameter = term("X<u32>");
    let constraints = prove(
        decls(),
        Env::default(),
        assumptions,
        Relation::WellFormed(goal),
    );
    constraints.assert_ok(
    expect!["{Constraints { env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}

#[test]
fn not_well_formed_adt() {
    let assumptions: Wcs = Wcs::t();
    let goal: Parameter = term("X<u64>");
    prove(
        decls(),
        Env::default(),
        assumptions,
        Relation::WellFormed(goal),
    )
    .assert_err(expect![[r#"
        the rule "trait implied bound" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}
