use crate::grammar::{Parameter, Predicate, Wcs};
use crate::rust::term;
use expect_test::expect;
use formality_core::test;
use std::sync::Arc;

use crate::prove::{decls::Program, prove, Env};

fn decls() -> Program {
    Program {
        crates: Arc::new(Program::program_from_items(vec![
            term("trait Foo where {}"),
            term("impl Foo for u32 {}"),
            term("struct X<T> where T : Foo {}"),
        ])),
        ..Program::empty()
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
        Predicate::WellFormed(goal),
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
        Predicate::WellFormed(goal),
    )
    .assert_err(expect![[r#"
        failed at (proven_set.rs) because
          no matching normalized forms

        the rule "trait implied bound" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}
