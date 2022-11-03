#![allow(non_snake_case)]

const PROGRAM: &str = "[
    crate core {
        trait Debug<> where [] {}
        trait Display<> where [] {}

        trait Get<> where [] {
            fn get<ty T, lt l>(&mut l T) -> () where [T: Debug<>];
        }

        impl<> Get<> for () where [] {
            fn get<ty T, lt l>(&mut l ()) -> () where [T: Debug<>] {
                trusted
            }
        }
    }
]";

#[test]
fn test_fn() {
    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait(Get)",
                source: "could not prove `well_formed_ty((rigid &(mut) !U(2)_1 !U(2)_0))`",
            },
        )
    "#]].assert_debug_eq(&formality_rust::test_program_ok(PROGRAM));
}
