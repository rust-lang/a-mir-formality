use crate::grammar::{Parameter, Relation, Wcs};
use crate::rust::term;
use expect_test::expect;
use formality_core::test;

use crate::prove::prove::{decls::Program, prove::prove, Env};

fn decls() -> Program {
    Program {
        crates: Program::program_from_items(vec![
            term("trait Foo where {}"),
            term("impl Foo for u32 {}"),
            term("struct X<T> where T : Foo {}"),
        ]),
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
        crates/formality-rust/src/prove/prove/prove/prove_via.rs:9:1: no applicable rules for prove_via { goal: u64 = u32, via: Foo(u64), assumptions: {Foo(u64)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u64, via: Foo(u64), assumptions: {Foo(u64)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:55:1: no applicable rules for prove_normalize_via { goal: u32, via: Foo(u64), assumptions: {Foo(u64)}, env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "trait implied bound" at (prove_wc.rs) failed because
          expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}
