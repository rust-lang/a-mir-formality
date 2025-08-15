// Decide how to do this test -- should it be a unit test?
// Can it be an integration test?

// const TEST_TY_IS_INT: &str = "[
//     crate test {
//         trait Id {
//             type This: [];
//         }

//         impl<ty T> Id for T {
//             type This = T;
//         }
//     }
// ]";

// #[test]
// fn test_ty_is_int() {
//     test_where_clause(
//         TEST_TY_IS_INT,
//         "{} => { <u16 as Id>::This = u16 }",
//     )
//     .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }}"]);

//     test_where_clause(
//         TEST_TY_IS_INT,
//         "{} => { @is_int(<u16 as Id>::This) }",
//     )
//     .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [], bias: Soundness, pending: [] }, known_true: true, substitution: {} }}"]);
// }
