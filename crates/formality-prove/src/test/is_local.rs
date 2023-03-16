use expect_test::expect;
use formality_macros::test;
use formality_types::parse::term;

use crate::decls::Decls;

use super::test_prove;

#[test]
fn test_forall_not_local() {
    let constraints = test_prove(
        Decls::empty(),
        term("coherence_mode {} => {for<ty T> @IsLocal(Debug(T))}"),
    );
    expect![[r#"
        {}
    "#]]
    .assert_debug_eq(&constraints);
}

#[test]
fn test_exists_not_local() {
    let constraints = test_prove(
        Decls::empty(),
        term("coherence_mode exists<ty T> {} => {@IsLocal(Debug(T))}"),
    );
    expect![[r#"
        {
            Constraints {
                env: Env {
                    variables: [
                        ?ty_1,
                    ],
                    coherence_mode: true,
                },
                known_true: false,
                substitution: {},
            },
        }
    "#]] // FIXME: really this should be ambiguous, not sure if it matters
    .assert_debug_eq(&constraints);
}
