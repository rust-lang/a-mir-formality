#![cfg(test)]

use expect_test;
use formality_types::{grammar::Hypothesis, parse::term};

use super::elaborate_hypotheses;
use crate::MockDatabase;

#[test]
fn test_single_step() {
    let db = MockDatabase::new()
        .with_invariant("<ty X> is_implemented(Ord(X)) => is_implemented(PartialOrd(X))")
        .into_db();

    let hypotheses: Vec<Hypothesis> = term(
        "
        [
            is_implemented(Ord(u32))
        ]
        ",
    );

    let hypotheses1 = elaborate_hypotheses(&db, &hypotheses);

    expect_test::expect![[r#"
        elaborated_hypotheses(
            {
                is_implemented(Ord((rigid (scalar u32)))),
                is_implemented(PartialOrd((rigid (scalar u32)))),
            },
        )
    "#]]
    .assert_debug_eq(&hypotheses1);
}

#[test]
fn test_transitive() {
    let db = MockDatabase::new()
        .with_invariant("<ty X> is_implemented(A(X)) => is_implemented(B(X))")
        .with_invariant("<ty X> is_implemented(B(X)) => is_implemented(C(X))")
        .into_db();

    let hypotheses: Vec<Hypothesis> = vec![term("is_implemented(A(u32))")];

    let hypotheses1 = elaborate_hypotheses(&db, &hypotheses);

    expect_test::expect![[r#"
        elaborated_hypotheses(
            {
                is_implemented(A((rigid (scalar u32)))),
                is_implemented(B((rigid (scalar u32)))),
                is_implemented(C((rigid (scalar u32)))),
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
