use formality::test_where_clause;

#[test]
fn test_mirror_normalizes_u32_to_u32() {
    // Variant of test_foo_crate_cannot_assume_CoreStruct_does_not_impl_CoreTrait
    // where there is a negative impl, so it is accepted.

    expect_test::expect![[r#"
        Ok(
            {
                Constraints {
                    env: Env {
                        variables: [
                            ?ty_1,
                        ],
                        coherence_mode: false,
                    },
                    known_true: true,
                    substitution: {
                        ?ty_1 => (rigid (scalar u32)),
                    },
                },
                Constraints {
                    env: Env {
                        variables: [
                            ?ty_1,
                        ],
                        coherence_mode: false,
                    },
                    known_true: true,
                    substitution: {
                        ?ty_1 => (alias (Mirror :: Assoc) (rigid (scalar u32))),
                    },
                },
            },
        )
    "#]]
    .assert_debug_eq(&test_where_clause(
        "[
            crate core {
                trait Mirror<> where [] {
                    type Assoc<> : [] where [];
                }

                impl<ty T> Mirror<> for T where [] {
                    type Assoc<> = T where [];
                }
            }
        ]",
        "exists<ty T> {} => {<u32 as Mirror>::Assoc<> = T}",
    ));
}
