use formality::test_where_clause;

const MIRROR: &str = "[
    crate core {
        safe trait Mirror<> where [] {
            type Assoc<> : [] where [];
        }

        safe impl<ty T> Mirror<> for T where [] {
            type Assoc<> = T where [];
        }
    }
]";

#[test]
fn test_mirror_normalizes_u32_to_u32() {
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
        MIRROR,
        "exists<ty T> {} => {<u32 as Mirror>::Assoc<> = T}",
    ));
}
