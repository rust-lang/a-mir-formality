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
        adt_decls: vec![term("adt X<ty T> where {Foo(T)}")],
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
    expect![[r#"
        {
            Constraints {
                env: Env {
                    variables: [],
                    bias: Soundness,
                },
                known_true: true,
                substitution: {},
            },
        }
    "#]]
    .assert_debug_eq(&constraints);
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
    ).assert_err(expect![[r#"
        judgment `prove { goal: {@ wf(X<u64>)}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)}], [], {}, {}) }` failed at the following rule(s):
          failed at (src/file.rs:LL:CC) because
            judgment `prove_wc_list { goal: {@ wf(X<u64>)}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                judgment `prove_wc { goal: @ wf(X<u64>), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                  the rule "parameter well formed" failed at step #0 (src/file.rs:LL:CC) because
                    judgment `prove_wf { goal: X<u64>, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                      the rule "ADT" failed at step #3 (src/file.rs:LL:CC) because
                        judgment `prove_after { constraints: Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }, goal: {Foo(u64)}, assumptions: {} }` failed at the following rule(s):
                          the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                            judgment `prove { goal: {Foo(u64)}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [trait Foo <ty> ], [impl Foo(u32)], [], [], [], [adt X <ty> where {Foo(^ty0_0)}], [], {}, {}) }` failed at the following rule(s):
                              failed at (src/file.rs:LL:CC) because
                                judgment `prove_wc_list { goal: {Foo(u64)}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                                    judgment `prove_wc { goal: Foo(u64), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                                      the rule "trait implied bound" failed at step #0 (src/file.rs:LL:CC) because
                                        expression evaluated to an empty collection: `decls.trait_invariants()`"#]]);
}
