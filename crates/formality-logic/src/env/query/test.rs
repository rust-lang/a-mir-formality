#![cfg(test)]

use crate::{env::Env, MockDatabase};

use super::{extract_query_result::extract_query_result, querify};
use formality_types::{
    grammar::{AtomicRelation, ElaboratedHypotheses, ScalarId, Ty},
    parse::{term, term_with},
};

fn create_test_env() -> (Env, Vec<(&'static str, Ty)>) {
    let mut env = Env::default();
    let (e0_0, e0_1): (Ty, Ty) = env.instantiate_existentially(&term("<ty A, ty B> (A, B)"));
    let (u1_0, u1_1): (Ty, Ty) = env.instantiate_universally(&term("<ty A, ty B> (A, B)"));
    let (e1_0, e1_1): (Ty, Ty) = env.instantiate_existentially(&term("<ty A, ty B> (A, B)"));
    let (u2_0, u2_1): (Ty, Ty) = env.instantiate_universally(&term("<ty A, ty B> (A, B)"));
    let (e2_0, e2_1): (Ty, Ty) = env.instantiate_existentially(&term("<ty A, ty B> (A, B)"));
    let (u3_0, u3_1): (Ty, Ty) = env.instantiate_universally(&term("<ty A, ty B> (A, B)"));
    let (e3_0, e3_1): (Ty, Ty) = env.instantiate_existentially(&term("<ty A, ty B> (A, B)"));
    let bindings = vec![
        ("E0_0", e0_0),
        ("E0_1", e0_1),
        ("E1_0", e1_0),
        ("E1_1", e1_1),
        ("E2_0", e2_0),
        ("E2_1", e2_1),
        ("E3_0", e3_0),
        ("E3_1", e3_1),
        ("U1_0", u1_0),
        ("U1_1", u1_1),
        ("U2_0", u2_0),
        ("U2_1", u2_1),
        ("U3_0", u3_0),
        ("U3_1", u3_1),
    ];

    (env, bindings)
}

// Test that we compress universes.
//
// W is renamed into U1 with index 0.
#[test]
fn test_compress_universes() {
    let (env, bindings) = create_test_env();
    let (_, substitution) = querify(
        &env,
        &ElaboratedHypotheses::none(),
        &term_with(&bindings, "is_implemented(Debug(U2_1))"),
    );
    expect_test::expect![[r#"
        VarSubstitution {
            map: {
                !tyU(1)_0: !tyU(2)_1,
            },
        }
    "#]]
    .assert_debug_eq(&substitution);
}

#[test]
fn test_placeholder_renamed_in_order_of_appearance() {
    // Key observations:
    //
    // W is renamed into U1 with index 0 (even though it originally had index 1).
    // V is renamed into U1 with index 1 (even though it originally had index 0).
    let (env, bindings) = create_test_env();
    let (_, substitution) = querify(
        &env,
        &ElaboratedHypotheses::none(),
        &term_with(&bindings, "is_implemented(Debug(U2_1, U2_0))"),
    );
    expect_test::expect![[r#"
        VarSubstitution {
            map: {
                !tyU(1)_0: !tyU(2)_1,
                !tyU(1)_1: !tyU(2)_0,
            },
        }
    "#]]
    .assert_debug_eq(&substitution);
}

#[test]
fn test_mix_universes() {
    // Test that T is mapped to U1 and X mapped from U3 to U2.
    let (env, bindings) = create_test_env();
    let (_, substitution) = querify(
        &env,
        &ElaboratedHypotheses::none(),
        &term_with(&bindings, "is_implemented(Debug(U1_0, U3_0))"),
    );
    expect_test::expect![[r#"
        VarSubstitution {
            map: {
                !tyU(1)_0: !tyU(1)_0,
                !tyU(2)_0: !tyU(3)_0,
            },
        }
    "#]]
    .assert_debug_eq(&substitution);
}

#[test]
fn test_existential_do_not_create_universe() {
    // Although F is created in a higher universe, it is mapped to U0 because there are no placeholders.
    let (env, bindings) = create_test_env();
    let (query, substitution) = querify(
        &env,
        &ElaboratedHypotheses::none(),
        &term_with(&bindings, "is_implemented(Debug(E2_0))"),
    );
    expect_test::expect![[r#"
        (
            env(
                U(0),
                [
                    inference_var_data(
                        ty,
                        U(0),
                        None,
                        [],
                        [],
                        [],
                        [],
                    ),
                ],
                no,
            ),
            VarSubstitution {
                map: {
                    ?ty0: ?ty4,
                },
            },
        )
    "#]]
    .assert_debug_eq(&(query.env, substitution));
}

#[test]
fn test_mix_existential_and_placeholder() {
    // Should map both E0, E1 to universe 0
    // Should map E3 to universe 1
    let (env, bindings) = create_test_env();
    let (query, substitution) = querify(
        &env,
        &ElaboratedHypotheses::none(),
        &term_with(&bindings, "is_implemented(Debug(E0_0, E1_1, U2_0, E3_0))"),
    );
    expect_test::expect![[r#"
        (
            env(
                U(1),
                [
                    inference_var_data(
                        ty,
                        U(0),
                        None,
                        [],
                        [],
                        [],
                        [],
                    ),
                    inference_var_data(
                        ty,
                        U(0),
                        None,
                        [],
                        [],
                        [],
                        [],
                    ),
                    inference_var_data(
                        ty,
                        U(1),
                        None,
                        [],
                        [],
                        [],
                        [],
                    ),
                ],
                no,
            ),
            VarSubstitution {
                map: {
                    !tyU(1)_0: !tyU(2)_0,
                    ?ty0: ?ty0,
                    ?ty1: ?ty3,
                    ?ty2: ?ty6,
                },
            },
        )
    "#]]
    .assert_debug_eq(&(query.env, substitution));
}

#[test]
fn test_query_result() {
    // Should map both E0, E1 to universe 0
    // Should map E3 to universe 1
    let (env, bindings) = create_test_env();
    let (query, substitution) = querify(
        &env,
        &ElaboratedHypotheses::none(),
        &term_with(&bindings, "is_implemented(Debug(E0_0, E1_1, U2_0, E3_0))"),
    );
    expect_test::expect![[r#"
        (
            env(
                U(1),
                [
                    inference_var_data(
                        ty,
                        U(0),
                        None,
                        [],
                        [],
                        [],
                        [],
                    ),
                    inference_var_data(
                        ty,
                        U(0),
                        None,
                        [],
                        [],
                        [],
                        [],
                    ),
                    inference_var_data(
                        ty,
                        U(1),
                        None,
                        [],
                        [],
                        [],
                        [],
                    ),
                ],
                no,
            ),
            VarSubstitution {
                map: {
                    !tyU(1)_0: !tyU(2)_0,
                    ?ty0: ?ty0,
                    ?ty1: ?ty3,
                    ?ty2: ?ty6,
                },
            },
        )
    "#]]
    .assert_debug_eq(&(query.env, substitution));
}

#[test]
fn test_query_result_noop() {
    // Should map both E0, E1 to universe 0
    // Should map E3 to universe 1
    let (env, bindings) = create_test_env();
    let (query, _substitution) = querify(
        &env,
        &ElaboratedHypotheses::none(),
        &term_with(&bindings, "is_implemented(Debug(E0_0, E1_1, U2_0, E3_0))"),
    );

    // test result when we do nothing -- answer, no-op subst
    let result = extract_query_result(&query, &query.env);
    expect_test::expect![[r#"
        query_result(
            <> query_result_bound_data([]),
        )
    "#]]
    .assert_debug_eq(&result);
}

#[test]
fn test_query_result_when_var_unified_with_i32() {
    // Should map both E0, E1 to universe 0
    // Should map E3 to universe 1
    let (env, bindings) = create_test_env();
    let (query, _substitution) = querify(
        &env,
        &ElaboratedHypotheses::none(),
        &term_with(&bindings, "is_implemented(Debug(E0_0, E1_1, U2_0, E3_0))"),
    );

    let final_env = query.env.clone();
    let env_vars = final_env.inference_variables();
    let qv0 = env_vars[0];

    let db = MockDatabase::empty();
    let assumptions = ElaboratedHypotheses::none();
    let (final_env, goals) = final_env
        .apply_relation(&db, &assumptions, &AtomicRelation::eq(qv0, ScalarId::I32))
        .unwrap();
    assert!(goals.is_empty());

    let result = extract_query_result(&query, &final_env);
    expect_test::expect![[r#"
        query_result(
            <> query_result_bound_data([equals(?ty0, (rigid (scalar i32)))]),
        )
    "#]]
    .assert_debug_eq(&result);
}
