use expect_test::expect;
use formality_macros::test;
use formality_types::rust::term;

use crate::decls::Decls;

use crate::test_util::test_prove;

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Decls {
    Decls {
        trait_decls: vec![term("trait Foo<ty Self> where {}")],
        impl_decls: vec![term("impl<ty T> Foo(Vec<T>) where {}")],
        ..Decls::empty()
    }
}

/// Test that `X = Vec<X>` cannot be solved
#[test]
fn direct_cycle() {
    test_prove(decls(), term("exists<ty A> {} => {A = Vec<A>}")).assert_err(
            expect![[r#"
                judgment `prove { goal: {?ty_0 = Vec<?ty_0>}, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness }, decls: decls(222, [trait Foo <ty> ], [impl <ty> Foo(Vec<^ty0_0>)], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                  failed at (src/file.rs:LL:CC) because
                    judgment `prove_wc_list { goal: {?ty_0 = Vec<?ty_0>}, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness } }` failed at the following rule(s):
                      the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_wc { goal: ?ty_0 = Vec<?ty_0>, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness } }` failed at the following rule(s):
                          the rule "eq" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `prove_eq { a: ?ty_0, b: Vec<?ty_0>, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness } }` failed at the following rule(s):
                              the rule "existential" failed at step #0 (src/file.rs:LL:CC) because
                                judgment `prove_existential_var_eq { v: ?ty_0, b: Vec<?ty_0>, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness } }` failed at the following rule(s):
                                  the rule "existential-nonvar" failed at step #1 (src/file.rs:LL:CC) because
                                    judgment `equate_variable` failed at the following rule(s):
                                      failed at (src/file.rs:LL:CC) because
                                        `?ty_0` occurs in `Vec<?ty_0>`"#]]);
}

/// Test that `X = Vec<Y>` can be solved
#[test]
fn eq_variable_to_rigid() {
    test_prove(decls(), term("exists<ty X, ty Y> {} => {X = Vec<Y>}")).assert_ok(expect![[r#"
        {
          Constraints { env: Env { variables: [?ty_3, ?ty_1, ?ty_2], bias: Soundness }, known_true: true, substitution: {?ty_1 => Vec<?ty_3>, ?ty_2 => ?ty_3} },
        }
    "#]]);
}

/// Test that `Vec<Y> = X` can be solved
#[test]
fn eq_rigid_to_variable() {
    test_prove(decls(), term("exists<ty X, ty Y> {} => {Vec<Y> = X}")).assert_ok(expect![[r#"
        {
          Constraints { env: Env { variables: [?ty_3, ?ty_1, ?ty_2], bias: Soundness }, known_true: true, substitution: {?ty_1 => Vec<?ty_3>, ?ty_2 => ?ty_3} },
        }
    "#]]);
}

/// Test that `X = Vec<X>` cannot be solved (when constructed over several steps)
#[test]
fn indirect_cycle_1() {
    test_prove(
        decls(),
        term("exists<ty A, ty B> {} => {A = Vec<B>, B = A}"),
    ).assert_err(
    expect![[r#"
        judgment `prove { goal: {?ty_0 = Vec<?ty_1>, ?ty_1 = ?ty_0}, assumptions: {}, env: Env { variables: [?ty_0, ?ty_1], bias: Soundness }, decls: decls(222, [trait Foo <ty> ], [impl <ty> Foo(Vec<^ty0_0>)], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
          failed at (src/file.rs:LL:CC) because
            judgment `prove_wc_list { goal: {?ty_0 = Vec<?ty_1>, ?ty_1 = ?ty_0}, assumptions: {}, env: Env { variables: [?ty_0, ?ty_1], bias: Soundness } }` failed at the following rule(s):
              the rule "some" failed at step #1 (src/file.rs:LL:CC) because
                judgment `prove_after { constraints: Constraints { env: Env { variables: [?ty_2, ?ty_0, ?ty_1], bias: Soundness }, known_true: true, substitution: {?ty_0 => Vec<?ty_2>, ?ty_1 => ?ty_2} }, goal: {?ty_1 = ?ty_0}, assumptions: {} }` failed at the following rule(s):
                  the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                    judgment `prove { goal: {?ty_0 = Vec<?ty_0>}, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness }, decls: decls(222, [trait Foo <ty> ], [impl <ty> Foo(Vec<^ty0_0>)], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                      failed at (src/file.rs:LL:CC) because
                        judgment `prove_wc_list { goal: {?ty_0 = Vec<?ty_0>}, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness } }` failed at the following rule(s):
                          the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `prove_wc { goal: ?ty_0 = Vec<?ty_0>, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness } }` failed at the following rule(s):
                              the rule "eq" failed at step #0 (src/file.rs:LL:CC) because
                                judgment `prove_eq { a: ?ty_0, b: Vec<?ty_0>, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness } }` failed at the following rule(s):
                                  the rule "existential" failed at step #0 (src/file.rs:LL:CC) because
                                    judgment `prove_existential_var_eq { v: ?ty_0, b: Vec<?ty_0>, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness } }` failed at the following rule(s):
                                      the rule "existential-nonvar" failed at step #1 (src/file.rs:LL:CC) because
                                        judgment `equate_variable` failed at the following rule(s):
                                          failed at (src/file.rs:LL:CC) because
                                            `?ty_0` occurs in `Vec<?ty_0>`"#]]);
}

/// Test that `X = Vec<X>` cannot be solved (when constructed over several steps)
#[test]
fn indirect_cycle_2() {
    test_prove(
        decls(),
        term("exists<ty A, ty B> {} => {B = A, A = Vec<B>}"),
    ).assert_err(
    expect![[r#"
        judgment `prove { goal: {?ty_0 = Vec<?ty_1>, ?ty_1 = ?ty_0}, assumptions: {}, env: Env { variables: [?ty_0, ?ty_1], bias: Soundness }, decls: decls(222, [trait Foo <ty> ], [impl <ty> Foo(Vec<^ty0_0>)], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
          failed at (src/file.rs:LL:CC) because
            judgment `prove_wc_list { goal: {?ty_0 = Vec<?ty_1>, ?ty_1 = ?ty_0}, assumptions: {}, env: Env { variables: [?ty_0, ?ty_1], bias: Soundness } }` failed at the following rule(s):
              the rule "some" failed at step #1 (src/file.rs:LL:CC) because
                judgment `prove_after { constraints: Constraints { env: Env { variables: [?ty_2, ?ty_0, ?ty_1], bias: Soundness }, known_true: true, substitution: {?ty_0 => Vec<?ty_2>, ?ty_1 => ?ty_2} }, goal: {?ty_1 = ?ty_0}, assumptions: {} }` failed at the following rule(s):
                  the rule "prove_after" failed at step #1 (src/file.rs:LL:CC) because
                    judgment `prove { goal: {?ty_0 = Vec<?ty_0>}, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness }, decls: decls(222, [trait Foo <ty> ], [impl <ty> Foo(Vec<^ty0_0>)], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
                      failed at (src/file.rs:LL:CC) because
                        judgment `prove_wc_list { goal: {?ty_0 = Vec<?ty_0>}, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness } }` failed at the following rule(s):
                          the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `prove_wc { goal: ?ty_0 = Vec<?ty_0>, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness } }` failed at the following rule(s):
                              the rule "eq" failed at step #0 (src/file.rs:LL:CC) because
                                judgment `prove_eq { a: ?ty_0, b: Vec<?ty_0>, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness } }` failed at the following rule(s):
                                  the rule "existential" failed at step #0 (src/file.rs:LL:CC) because
                                    judgment `prove_existential_var_eq { v: ?ty_0, b: Vec<?ty_0>, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness } }` failed at the following rule(s):
                                      the rule "existential-nonvar" failed at step #1 (src/file.rs:LL:CC) because
                                        judgment `equate_variable` failed at the following rule(s):
                                          failed at (src/file.rs:LL:CC) because
                                            `?ty_0` occurs in `Vec<?ty_0>`"#]]);
}
