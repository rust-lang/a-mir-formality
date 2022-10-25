#![cfg(test)]

use super::querify;
use crate::{
    env::Env,
    grammar::Ty,
    parse::{term, term_with},
};
use test_log::test;

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
    let (_, substitution) = querify(&env, &term_with(&bindings, "is_implemented(Debug(U2_1))"));
    expect_test::expect![[r#"
        VarSubstitution {
            map: {
                !U(1)_0: !U(2)_1,
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
        &term_with(&bindings, "is_implemented(Debug(U2_1, U2_0))"),
    );
    expect_test::expect![[r#"
        VarSubstitution {
            map: {
                !U(1)_0: !U(2)_1,
                !U(1)_1: !U(2)_0,
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
        &term_with(&bindings, "is_implemented(Debug(U1_0, U3_0))"),
    );
    expect_test::expect![[r#"
        VarSubstitution {
            map: {
                !U(1)_0: !U(1)_0,
                !U(2)_0: !U(3)_0,
            },
        }
    "#]]
    .assert_debug_eq(&substitution);
}

#[test]
fn test_existential_do_not_create_universe() {
    // Although F is created in a higher universe, it is mapped to U0 because there are no placeholders.
    let (env, bindings) = create_test_env();
    let (query, substitution) = querify(&env, &term_with(&bindings, "is_implemented(Debug(E2_0))"));
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
                    ?0: ?4,
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
                    !U(1)_0: !U(2)_0,
                    ?0: ?0,
                    ?1: ?3,
                    ?2: ?6,
                },
            },
        )
    "#]]
    .assert_debug_eq(&(query.env, substitution));
}
