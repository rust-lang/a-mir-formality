use a_mir_formality::test_where_clause;
use formality_core::test_util::ResultTestExt;

const EFFECT_PREFIX: &str = "[
    crate test {
        trait Trait {
        }
    }
]";

#[test]
fn test_runtime_subset_const() {
    test_where_clause(EFFECT_PREFIX, "{} => {@subset(runtime, const)}")
        .assert_err(expect_test::expect![[r#"
            judgment `prove { goal: {@ subset(runtime , const)}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [trait Trait <ty> ], [], [], [], [], [], {Trait}, {}) }` failed at the following rule(s):
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


#[test]
fn test_const_subset_runtime() {
    test_where_clause(EFFECT_PREFIX, "{} => {@subset(const, runtime)}")
        .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} }}"]);
}
