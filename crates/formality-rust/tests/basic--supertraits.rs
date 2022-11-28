#![allow(non_snake_case)]

const PROGRAM: &str = "[
    crate core {
        trait Eq<> where [Self: PartialEq<>] { }
        trait PartialEq<> where [] { }

        // ComparableBase is a supertype, but `T: Eq` is not.
        trait Comparable<ty T> where [T: Eq<>, Self: ComparableBase<>] { }
        trait ComparableBase<> where [] { }
    }
]";

#[test]
fn test_implies() {
    expect_test::expect![[r#"
        Ok(
            yes,
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_can_prove_goal(
        PROGRAM,
        "for_all(<ty T> implies([is_implemented(Eq(T))], is_implemented(PartialEq(T))))",
    ));
}

#[test]
fn test_implies_rev() {
    expect_test::expect![[r#"
        Ok(
            no,
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_can_prove_goal(
        PROGRAM,
        "for_all(<ty T> implies([is_implemented(PartialEq(T))], is_implemented(Eq(T))))",
    ));
}

#[test]
fn test_non_supertrait_not_implied() {
    expect_test::expect![[r#"
        Ok(
            no,
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_can_prove_goal(
        PROGRAM,
        "for_all(<ty T, ty U> implies([is_implemented(Comparable(T, U))], is_implemented(Eq(U))))",
    ));
}

#[test]
fn test_comparable_implies_comparable_base() {
    expect_test::expect![[r#"
        Ok(
            yes,
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_can_prove_goal(
        PROGRAM,
        "for_all(<ty T, ty U> implies([is_implemented(Comparable(T, U))], is_implemented(ComparableBase(T))))",
    ));
}
