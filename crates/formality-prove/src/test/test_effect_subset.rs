use std::sync::Arc;

use expect_test::expect;
use formality_macros::test;
use formality_types::grammar::{AtomicEffect, Effect, Relation, Wcs};

use crate::{decls::Decls, prove::prove, Env};

#[test]
fn test_effect_subset_success() {
    let runtime_eff = Effect::Atomic(AtomicEffect::Runtime);
    let const_eff = Effect::Atomic(AtomicEffect::Const);
    let runtime_const_union =
        Effect::Union(Arc::new(runtime_eff.clone()), Arc::new(const_eff.clone()));
    // Test (runtime) <: (runtime, const)
    let constraint_0 = prove(
        Decls::empty(),
        Env::default(),
        Wcs::t(),
        Relation::EffectSubset(runtime_eff.clone(), runtime_const_union.clone()),
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
    .assert_debug_eq(&constraint_0);
    // Test (const) <: (runtime, const)
    let constraint_1 = prove(
        Decls::empty(),
        Env::default(),
        Wcs::t(),
        Relation::EffectSubset(const_eff.clone(), runtime_const_union.clone()),
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
    .assert_debug_eq(&constraint_1);
    // Test (const) <: (runtime)
    let constraint_2 = prove(
        Decls::empty(),
        Env::default(),
        Wcs::t(),
        Relation::EffectSubset(const_eff.clone(), runtime_eff.clone()),
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
    .assert_debug_eq(&constraint_2);
    // Test (const, runtime) <: (runtime)
    let constraint_3 = prove(
        Decls::empty(),
        Env::default(),
        Wcs::t(),
        Relation::EffectSubset(runtime_const_union.clone(), runtime_eff.clone()),
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
    .assert_debug_eq(&constraint_3);
}

#[test]
fn test_effect_subset_failure() {
    let runtime_eff = Effect::Atomic(AtomicEffect::Runtime);
    let const_eff = Effect::Atomic(AtomicEffect::Const);

    // Runtime is not subset of const.
    // FIXME: the test output can be better?
    let constraint = prove(
        Decls::empty(),
        Env::default(),
        Wcs::t(),
        Relation::EffectSubset(runtime_eff.clone(), const_eff.clone()),
    );
    expect![[r#"
        FailedJudgment {
            judgment: "prove { goal: {@ subset(runtime , const)}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [], [], [], [], [], [], {}, {}) }",
            failed_rules: {
                FailedRule {
                    rule_name_index: None,
                    file: "crates/formality-prove/src/prove.rs",
                    line: 70,
                    column: 59,
                    cause: FailedJudgment(
                        FailedJudgment {
                            judgment: "prove_wc_list { goal: {@ subset(runtime , const)}, assumptions: {}, env: Env { variables: [], bias: Soundness } }",
                            failed_rules: {
                                FailedRule {
                                    rule_name_index: Some(
                                        (
                                            "some",
                                            0,
                                        ),
                                    ),
                                    file: "crates/formality-prove/src/prove/prove_wc_list.rs",
                                    line: 28,
                                    column: 14,
                                    cause: FailedJudgment(
                                        FailedJudgment {
                                            judgment: "prove_wc { goal: @ subset(runtime , const), assumptions: {}, env: Env { variables: [], bias: Soundness } }",
                                            failed_rules: {
                                                FailedRule {
                                                    rule_name_index: Some(
                                                        (
                                                            "effect subset",
                                                            0,
                                                        ),
                                                    ),
                                                    file: "crates/formality-prove/src/prove/prove_wc.rs",
                                                    line: 138,
                                                    column: 14,
                                                    cause: FailedJudgment(
                                                        FailedJudgment {
                                                            judgment: "prove_effect_subset { subset: runtime, superset: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }",
                                                            failed_rules: {
                                                                FailedRule {
                                                                    rule_name_index: Some(
                                                                        (
                                                                            "atomic",
                                                                            0,
                                                                        ),
                                                                    ),
                                                                    file: "crates/formality-prove/src/prove/prove_effect_subset.rs",
                                                                    line: 39,
                                                                    column: 14,
                                                                    cause: FailedJudgment(
                                                                        FailedJudgment {
                                                                            judgment: "prove_atomic_effect_subset { atomic_subeffect: runtime, superset: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }",
                                                                            failed_rules: {
                                                                                FailedRule {
                                                                                    rule_name_index: Some(
                                                                                        (
                                                                                            "transitive",
                                                                                            0,
                                                                                        ),
                                                                                    ),
                                                                                    file: "crates/formality-prove/src/prove/prove_effect_subset.rs",
                                                                                    line: 70,
                                                                                    column: 14,
                                                                                    cause: FailedJudgment(
                                                                                        FailedJudgment {
                                                                                            judgment: "prove_effect_upper_bound { f1: runtime, assumptions: {}, env: Env { variables: [], bias: Soundness } }",
                                                                                            failed_rules: {},
                                                                                        },
                                                                                    ),
                                                                                },
                                                                                FailedRule {
                                                                                    rule_name_index: Some(
                                                                                        (
                                                                                            "union-subset-lhs",
                                                                                            1,
                                                                                        ),
                                                                                    ),
                                                                                    file: "crates/formality-prove/src/prove/prove_effect_subset.rs",
                                                                                    line: 62,
                                                                                    column: 14,
                                                                                    cause: FailedJudgment(
                                                                                        FailedJudgment {
                                                                                            judgment: "prove_atomic_effect_eq { f1: runtime, f2: const, assumptions: {}, env: Env { variables: [], bias: Soundness } }",
                                                                                            failed_rules: {},
                                                                                        },
                                                                                    ),
                                                                                },
                                                                            },
                                                                        },
                                                                    ),
                                                                },
                                                            },
                                                        },
                                                    ),
                                                },
                                            },
                                        },
                                    ),
                                },
                            },
                        },
                    ),
                },
            },
        }
    "#]]
    .assert_debug_eq(&constraint);
}

#[test]
fn three_atomic_test() {
    // runtime <: Union(Union(const, const), runtime)
    let runtime_eff = Effect::Atomic(AtomicEffect::Runtime);
    let const_eff = Effect::Atomic(AtomicEffect::Const);
    let union0 = Effect::Union(Arc::new(const_eff.clone()), Arc::new(const_eff.clone()));
    let union1 = Effect::Union(Arc::new(union0), Arc::new(runtime_eff.clone()));
    let constraint_0 = prove(
        Decls::empty(),
        Env::default(),
        Wcs::t(),
        Relation::EffectSubset(runtime_eff, union1),
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
    .assert_debug_eq(&constraint_0);
}
