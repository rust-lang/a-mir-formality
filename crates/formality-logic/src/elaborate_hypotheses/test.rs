#![cfg(test)]
#![cfg(FIXME)]
use expect_test;
use formality_types::{grammar::Hypothesis, parse::term};

use super::elaborate_hypotheses;
use crate::MockDatabase;

#[test]
fn test_single_step() {
    let db = MockDatabase::new()
        .with_invariant("<ty X> Ord(X) => PartialOrd(X)")
        .into_db();

    let hypotheses: Vec<Hypothesis> = term(
        "
        [
            Ord(u32)
        ]
        ",
    );

    let hypotheses1 = elaborate_hypotheses(&db, &hypotheses);

    expect_test::expect![[r#"
        elaborated_hypotheses(
            {
                Ord((rigid (scalar u32))),
                PartialOrd((rigid (scalar u32))),
            },
        )
    "#]]
    .assert_debug_eq(&hypotheses1);
}

#[test]
fn test_transitive() {
    let db = MockDatabase::new()
        .with_invariant("<ty X> A(X) => B(X)")
        .with_invariant("<ty X> B(X) => C(X)")
        .into_db();

    let hypotheses: Vec<Hypothesis> = vec![term("A(u32)")];

    let hypotheses1 = elaborate_hypotheses(&db, &hypotheses);

    expect_test::expect![[r#"
        elaborated_hypotheses(
            {
                A((rigid (scalar u32))),
                B((rigid (scalar u32))),
                C((rigid (scalar u32))),
            },
        )
    "#]]
    .assert_debug_eq(&hypotheses1);
}

#[test]
fn test_well_formed_adt() {
    let db = MockDatabase::new().into_db();

    let hypotheses: Vec<Hypothesis> = vec![term("well_formed(Vec<u32>)")];

    let hypotheses1 = elaborate_hypotheses(&db, &hypotheses);

    expect_test::expect![[r#"
        elaborated_hypotheses(
            {
                well_formed_adt(Vec, [(rigid (scalar u32))]),
                well_formed((rigid (adt Vec) (rigid (scalar u32)))),
            },
        )
    "#]]
    .assert_debug_eq(&hypotheses1);
}

#[test]
fn test_well_formed_ref() {
    let db = MockDatabase::new().into_db();

    let hypotheses: Vec<Hypothesis> = vec![term("well_formed(& static u32)")];

    let hypotheses1 = elaborate_hypotheses(&db, &hypotheses);

    expect_test::expect![[r#"
        elaborated_hypotheses(
            {
                outlives((rigid (scalar u32)), static),
                well_formed((rigid &(shared) static (rigid (scalar u32)))),
            },
        )
    "#]]
    .assert_debug_eq(&hypotheses1);
}

#[test]
fn test_rigid_outlives() {
    let db = MockDatabase::new().into_db();

    let hypotheses: Vec<Hypothesis> = vec![term("outlives(Vec<u32>, static)")];

    let hypotheses1 = elaborate_hypotheses(&db, &hypotheses);

    expect_test::expect![[r#"
        elaborated_hypotheses(
            {
                outlives((rigid (adt Vec) (rigid (scalar u32))), static),
                outlives((rigid (scalar u32)), static),
            },
        )
    "#]]
    .assert_debug_eq(&hypotheses1);
}
