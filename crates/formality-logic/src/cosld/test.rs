#![cfg(test)]

use formality_macros::test;
use formality_types::{
    collections::Set,
    grammar::{AtomicRelation, Binder, Goal, Universe},
    parse::term,
};

use super::CosldResult;
use crate::env::Env;
use crate::MockDatabase;

#[test]
fn simple_test() {
    let db = MockDatabase::new()
        .with_program_clause(
            "for_all(<ty T> implies([is_implemented(Debug(T))], is_implemented(Debug(Vec<T>))))",
        )
        .with_program_clause("is_implemented(Debug(u32))")
        .into_db();
    let env = Env::default();

    let results = super::prove(&db, &env, &[], &term("is_implemented(Debug(Vec<u32>))"));

    expect_test::expect![[r#"
            {
                yes(
                    env(
                        U(0),
                        [
                            inference_var_data(
                                ty,
                                U(0),
                                Some(
                                    (rigid (scalar u32)),
                                ),
                                [],
                                [],
                                [],
                                [],
                            ),
                        ],
                        no,
                    ),
                ),
            }
        "#]]
    .assert_debug_eq(&results);
}

fn extract_relations(s: &Set<CosldResult>, universe: Universe) -> Vec<Option<Vec<AtomicRelation>>> {
    s.iter()
        .map(|r| match r {
            CosldResult::Maybe => None,
            CosldResult::Yes(env) => Some(env.inference_var_relations(universe)),
        })
        .collect()
}

#[test]
fn outlives_refs() {
    let db = MockDatabase::new().into_db();
    let mut env = Env::default();

    let b_goal: Binder<Goal> = term("<lt a, lt b> sub(&a u32, &b u32)");
    let goal = env.instantiate_existentially(&b_goal);
    let universe = env.term_universe(&goal);
    let results = super::prove(&db, &env, &[], &goal);

    expect_test::expect![[r#"
        [
            Some(
                [
                    outlives(
                        ?lt0,
                        ?lt1,
                    ),
                ],
            ),
        ]
    "#]]
    .assert_debug_eq(&extract_relations(&results, universe));
}

#[test]
fn outlives_assoc_type() {
    let db = MockDatabase::new().into_db();
    let mut env = Env::default();

    let b_goal: Binder<Goal> = term("<lt a, lt b> outlives(<u32 as Foo<a>>::Item, b)");
    let goal = env.instantiate_existentially(&b_goal);
    let results = super::prove(&db, &env, &[], &goal);

    expect_test::expect![[r#"
        [
            Some(
                [
                    outlives(
                        ?lt0,
                        ?lt1,
                    ),
                ],
            ),
        ]
    "#]]
    .assert_debug_eq(&extract_relations(&results, Universe::ROOT));
}

#[test]
fn outlives_assoc_type_normalizes() {
    let db = MockDatabase::new()
        .with_program_clause("for_all(<ty T, lt a> normalizes_to((alias (Foo::Item) T a), T))")
        .into_db();
    let mut env = Env::default();

    let b_goal: Binder<Goal> = term("<lt a, lt b> outlives(<u32 as Foo<a>>::Item, b)");
    let goal = env.instantiate_existentially(&b_goal);
    let universe = env.term_universe(&goal);
    let results = super::prove(&db, &env, &[], &goal);

    // The first result is when we successfully normalize.
    // Note that there are no outlives obligations.
    // The second result is when we do NOT normalize.
    // We do produce outlives obligations.
    expect_test::expect![[r#"
        [
            Some(
                [
                    equals(
                        ?ty2,
                        (rigid (scalar u32)),
                    ),
                    equals(
                        ?ty3,
                        (rigid (scalar u32)),
                    ),
                    equals(
                        ?lt4,
                        ?lt0,
                    ),
                ],
            ),
            Some(
                [
                    outlives(
                        ?lt0,
                        ?lt1,
                    ),
                ],
            ),
        ]
    "#]]
    .assert_debug_eq(&extract_relations(&results, universe));
}

#[test]
fn outlives_placeholder_no_facts() {
    let db = MockDatabase::new()
        .with_program_clause("for_all(<ty T, lt a> normalizes_to((alias (Foo::Item) T a), T))")
        .into_db();
    let mut env = Env::default();

    let goal: Binder<Goal> = term("<lt a> for_all(<lt c> outlives(a, c))");
    let goal = env.instantiate_existentially(&goal);
    let universe = env.term_universe(&goal);

    let results = super::prove(&db, &env, &[], &goal);

    // The first result is when we successfully normalize.
    // Note that there are no outlives obligations.
    // The second result is when we do NOT normalize.
    // We do produce outlives obligations.
    expect_test::expect![[r#"
        [
            Some(
                [
                    outlives(
                        ?lt0,
                        static,
                    ),
                ],
            ),
        ]
    "#]]
    .assert_debug_eq(&extract_relations(&results, universe));
}

#[test]
fn outlives_placeholder_implies_a_c() {
    let db = MockDatabase::new()
        .with_program_clause("for_all(<ty T, lt a> normalizes_to((alias (Foo::Item) T a), T))")
        .into_db();
    let mut env = Env::default();

    let goal: Binder<Goal> =
        term("<lt a> for_all(<lt c> implies([outlives(a, c)], outlives(a, c)))");
    let goal = env.instantiate_existentially(&goal);
    let universe = env.term_universe(&goal);

    let results = super::prove(&db, &env, &[], &goal);

    // The first result is when we successfully normalize.
    // Note that there are no outlives obligations.
    // The second result is when we do NOT normalize.
    // We do produce outlives obligations.
    expect_test::expect![[r#"
        [
            Some(
                [
                    outlives(
                        ?lt0,
                        ?lt0,
                    ),
                ],
            ),
        ]
    "#]]
    .assert_debug_eq(&extract_relations(&results, universe));
}

#[test]
fn outlives_placeholder_implies_b_c() {
    let db = MockDatabase::new()
        .with_program_clause("for_all(<ty T, lt a> normalizes_to((alias (Foo::Item) T a), T))")
        .into_db();
    let mut env = Env::default();

    let goal: Binder<Goal> =
        term("<lt a, lt b> for_all(<lt c> implies([outlives(b, c)], outlives(a, c)))");
    let goal = env.instantiate_existentially(&goal);
    let universe = env.term_universe(&goal);

    let results = super::prove(&db, &env, &[], &goal);

    // The first result is when we successfully normalize.
    // Note that there are no outlives obligations.
    // The second result is when we do NOT normalize.
    // We do produce outlives obligations.
    expect_test::expect![[r#"
        [
            Some(
                [
                    outlives(
                        ?lt0,
                        ?lt1,
                    ),
                ],
            ),
        ]
    "#]]
    .assert_debug_eq(&extract_relations(&results, universe));
}

#[test]
fn outlives_placeholder_implies_b_d_c() {
    let db = MockDatabase::new().into_db();
    let mut env = Env::default();

    let goal: Binder<Goal> =
        term("<lt a, lt b> for_all(<lt c, lt d> implies([outlives(d, c), outlives(b, d)], outlives(a, c)))");
    let goal = env.instantiate_existentially(&goal);
    let universe = env.term_universe(&goal);

    let results = super::prove(&db, &env, &[], &goal);

    // The first result is when we successfully normalize.
    // Note that there are no outlives obligations.
    // The second result is when we do NOT normalize.
    // We do produce outlives obligations.
    expect_test::expect![[r#"
        [
            Some(
                [
                    outlives(
                        ?lt0,
                        ?lt1,
                    ),
                ],
            ),
        ]
    "#]]
    .assert_debug_eq(&extract_relations(&results, universe));
}

#[test]
fn outlives_placeholder_implies_complex() {
    let db = MockDatabase::new().into_db();
    let mut env = Env::default();

    let goal: Binder<Goal> =
        term("<lt a, lt b> for_all(<lt c, lt d> implies([for_all(<lt x> implies([outlives(x, b)], outlives(x, c)))], outlives(a, c)))");
    let goal = env.instantiate_existentially(&goal);
    let universe = env.term_universe(&goal);

    let results = super::prove(&db, &env, &[], &goal);

    expect_test::expect![[r#"
        [
            Some(
                [
                    outlives(
                        ?lt0,
                        static,
                    ),
                ],
            ),
            Some(
                [
                    outlives(
                        ?lt0,
                        ?lt1,
                    ),
                ],
            ),
        ]
    "#]]
    .assert_debug_eq(&extract_relations(&results, universe));
}

#[test]
fn outlives_placeholder_implies_inf_var_in_env() {
    let db = MockDatabase::new().into_db();
    let mut env = Env::default();

    let goal: Binder<Goal> =
        term("<lt a, lt b> for_all(<lt c, lt d> implies([for_all(<lt x> implies([outlives(x, b)], outlives(x, c)))], equals(a, c)))");
    let goal = env.instantiate_existentially(&goal);
    let universe = env.term_universe(&goal);

    let results = super::prove(&db, &env, &[], &goal);

    expect_test::expect![[r#"
        []
    "#]]
    .assert_debug_eq(&extract_relations(&results, universe));
}

#[test]
fn outlives_placeholder_implies_silly() {
    let db = MockDatabase::new().into_db();
    let mut env = Env::default();

    let goal: Binder<Goal> =
        term("<lt a, lt b> for_all(<lt c> implies([outlives(c, c)], equals(a, c)))");
    let goal = env.instantiate_existentially(&goal);
    let universe = env.term_universe(&goal);

    let results = super::prove(&db, &env, &[], &goal);

    expect_test::expect![[r#"
        []
    "#]]
    .assert_debug_eq(&extract_relations(&results, universe));
}
