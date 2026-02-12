use crate::grammar::{Parameter, Relation, Wcs};
use crate::rust::term;
use expect_test::expect;
use formality_core::test;

use crate::prove::prove::{decls::Decls, prove::prove, Env};

fn decls() -> Decls {
    Decls {
        program: Decls::program_from_items(vec![
            term("trait Foo where {}"),
            term("impl Foo for u32 {}"),
            term("struct X<ty T> where T : Foo {}"),
        ]),
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
