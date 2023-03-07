#![cfg(FIXME)]
#![allow(non_snake_case)]

#[test]
#[ignore]
fn test_universal() {
    expect_test::expect![[r#"
        Err(
            Error {
                context: "check_trait(WellFormed)",
                source: Error {
                    context: "prove_where_clause_well_formed([for_all(<ty> is_implemented(A((rigid (scalar u32)), ^ty0_0)))] => for_all(<ty> is_implemented(A((rigid (scalar u32)), ^ty0_0)))",
                    source: Error {
                        context: "prove_where_clause_well_formed([for_all(<ty> is_implemented(A((rigid (scalar u32)), ^ty0_0)))] => is_implemented(A((rigid (scalar u32)), !tyU(2)_0))",
                        source: "could not prove `well_formed_trait_ref(A((rigid (scalar u32)), !tyU(2)_0))` given `[\n    for_all(<ty> is_implemented(A((rigid (scalar u32)), ^ty0_0))),\n]`",
                    },
                },
            },
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_program_ok(
        "[
            crate core {
                trait A<ty T> where [T: B<>] { }

                trait B<> where [] { }

                trait WellFormed<> where [for<ty T> u32: A<T>] { }
            }
        ]",
    ));

    expect_test::expect![[r#"
        Ok(
            (),
        )
    "#]]
    .assert_debug_eq(&formality_rust::test_program_ok(
        "[
            crate core {
                trait A<ty T> where [T: B<>] { }

                trait B<> where [] { }

                trait WellFormed<> where [for<ty T> u32: A<T>] { }

                impl <ty T> B<> for T where [] {}
            }
        ]",
    ));
}
