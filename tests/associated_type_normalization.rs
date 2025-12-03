use a_mir_formality::test_where_clause;
use formality_core::test;

const MIRROR: &str = "[
    crate core {
        trait Mirror {
            type Assoc : [];
        }

        impl<ty T> Mirror for T {
            type Assoc = T;
        }
    }
]";

#[test]
fn test_mirror_normalizes_u32_to_u32() {
    test_where_clause(MIRROR, "exists<ty T> {} => {<u32 as Mirror>::Assoc = T}")
        .assert_ok(expect_test::expect!["{Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_1 => u32} }, Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {?ty_1 => <u32 as Mirror>::Assoc} }}"]);
}
