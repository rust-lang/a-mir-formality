#![cfg(test)]

use expect_test;
use formality_types::{db::mock::MockDatabase, grammar::Hypothesis, parse::term};

use super::elaborate_hypotheses;

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
        {
            is_implemented(Ord((rigid (scalar u32)))),
            is_implemented(PartialOrd((rigid (scalar u32)))),
        }
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
        {
            is_implemented(A((rigid (scalar u32)))),
            is_implemented(B((rigid (scalar u32)))),
            is_implemented(C((rigid (scalar u32)))),
        }
    "#]]
    .assert_debug_eq(&hypotheses1);
}
