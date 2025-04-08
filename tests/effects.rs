use a_mir_formality::test_where_clause;
use formality_core::test_util::ResultTestExt;
use formality_types::grammar::{TraitId, TraitRef};

// FIXME: we don't need this for the current set of test to pass, but we might eventually need this, so keep this here. 
const EFFECT_PREFIX: &str = "[
    crate test {
        trait Foo {
        }
    }
]";

// Basic tests for const-runtime relation that should pass.
#[test]
fn test_const_runtime_basic() {
    // (const) <: (runtime)
    test_where_clause(EFFECT_PREFIX, "{} => {@subset(const, runtime)}")
        .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }}"]);

    // (runtime) <: (runtime, const)
    test_where_clause(EFFECT_PREFIX, "{} => {@subset(runtime, union(runtime, const))}")
        .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }}"]);

    // (const) <: (runtime, const)
    test_where_clause(EFFECT_PREFIX, "{} => {@subset(const, union(runtime, const))}")
        .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }}"]);

    // (const, runtime) <: (runtime)
    test_where_clause(EFFECT_PREFIX, "{} => {@subset(union(const, runtime), runtime)}")
        .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }}"]);
}

// Runtime is not subset of const, so this test should fail.
#[test]
fn test_runtime_subset_const() {
    test_where_clause(EFFECT_PREFIX, "{} => {@subset(runtime, const)}")
        .assert_err(expect_test::expect![[r#"
            judgment `prove { goal: {@ subset(runtime , const)}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [trait Foo <ty> ], [], [], [], [], [], {Foo}, {}) }` failed at the following rule(s):
              failed at (src/file.rs:LL:CC) because
                judgment `prove_wc_list { goal: {@ subset(runtime , const)}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                    judgment `prove_wc { goal: @ subset(runtime , const), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                      the rule "effect subset" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_effect_subset { subset: runtime, superset: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                          the rule "atomic" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `prove_atomic_effect_subset { atomic_subeffect: runtime, superset: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                              the rule "transitive" failed at step #0 (src/file.rs:LL:CC) because
                                judgment had no applicable rules: `prove_effect_upper_bound { f1: runtime, assumptions: {}, env: Env { variables: [], bias: Soundness } }`
                              the rule "union-subset-lhs" failed at step #1 (src/file.rs:LL:CC) because
                                judgment had no applicable rules: `prove_atomic_effect_eq { f1: runtime, f2: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }`"#]]);
}

// Test if the rule is still correct when there is more than two atomic effects. 
#[test]
fn test_three_atomic_effect() {
    //union(union(const, const), runtime) <: runtime
    test_where_clause(EFFECT_PREFIX, "{} => {@subset(union(union(const, const), runtime), runtime)}")
        .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }}"]);
}

#[test]
fn test_associated_effect() {
    // A runtime associated effect is subset of runtime. 
    test_where_clause(EFFECT_PREFIX, "{} => {@ subset(AssociatedEffect(Foo()) , runtime)}")
        .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }}"]);

    
    // An associated effect is subset of itself.
    test_where_clause(EFFECT_PREFIX, "{} => {@ subset(AssociatedEffect(Foo()) , AssociatedEffect(Foo()))}")
        .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }}"]);
    
}

#[test]
fn test_assoc_runtime_not_subset_of_const() {
    // A runtime associated effect is NOT subset of const.
    test_where_clause(EFFECT_PREFIX, "{} => {@ subset(AssociatedEffect(Foo()) , const)}")
        .assert_err(expect_test::expect![[r#"
            judgment `prove { goal: {@ subset(AssociatedEffect(Foo()) , const)}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [trait Foo <ty> ], [], [], [], [], [], {Foo}, {}) }` failed at the following rule(s):
              failed at (src/file.rs:LL:CC) because
                judgment `prove_wc_list { goal: {@ subset(AssociatedEffect(Foo()) , const)}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                  the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                    judgment `prove_wc { goal: @ subset(AssociatedEffect(Foo()) , const), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                      the rule "effect subset" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_effect_subset { subset: AssociatedEffect(Foo()), superset: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                          the rule "associated effect" failed at step #1 (src/file.rs:LL:CC) because
                            judgment `prove_effect_subset { subset: runtime, superset: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                              the rule "atomic" failed at step #0 (src/file.rs:LL:CC) because
                                judgment `prove_atomic_effect_subset { atomic_subeffect: runtime, superset: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                                  the rule "transitive" failed at step #0 (src/file.rs:LL:CC) because
                                    judgment had no applicable rules: `prove_effect_upper_bound { f1: runtime, assumptions: {}, env: Env { variables: [], bias: Soundness } }`
                                  the rule "union-subset-lhs" failed at step #1 (src/file.rs:LL:CC) because
                                    judgment had no applicable rules: `prove_atomic_effect_eq { f1: runtime, f2: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }`
                          the rule "atomic" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `prove_atomic_effect_subset { atomic_subeffect: AssociatedEffect(Foo()), superset: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                              the rule "transitive" failed at step #0 (src/file.rs:LL:CC) because
                                judgment had no applicable rules: `prove_effect_upper_bound { f1: AssociatedEffect(Foo()), assumptions: {}, env: Env { variables: [], bias: Soundness } }`
                              the rule "union-subset-lhs" failed at step #1 (src/file.rs:LL:CC) because
                                judgment had no applicable rules: `prove_atomic_effect_eq { f1: AssociatedEffect(Foo()), f2: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }`"#]]);
}
