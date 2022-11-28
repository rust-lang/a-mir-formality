#![allow(non_snake_case)]

const PROGRAM: &str = "[
    crate core {
        trait Sub<> where [for<lt l> Self: Super<l>] { }
        trait Super<lt x> where [] { }
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
        "for_all(<ty T> implies([is_implemented(Sub(T))], for_all(<lt x> is_implemented(Super(T, x)))))",
    ));
}
