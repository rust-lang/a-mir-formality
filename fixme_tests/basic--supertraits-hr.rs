#![cfg(FIXME)]
#![allow(non_snake_case)]

const PROGRAM: &str = "[
    crate core {
        trait Sub where for<lt l> Self: Super<l> { }
        trait Super<lt x> { }
    }
]";

#[test]
#[ignore]
fn test_implies() {
    expect_test::expect![[r#"
        Ok(
            yes,
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_can_prove_goal(
        PROGRAM,
        "for_all(<ty T> implies([Sub(T)], for_all(<lt x> Super(T, x))))",
    ));
}
