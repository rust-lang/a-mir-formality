use expect_test::expect;
use formality_macros::test;
use formality_types::rust::term;

use crate::decls::Decls;

use crate::test_util::test_prove;

#[test]
fn test_forall_not_local() {
    test_prove(Decls::empty(), term("{} => {for<ty T> @IsLocal(Debug(T))}")).assert_err(expect![[
        r#"
        the rule "local trait" at (is_local.rs) failed because
          condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
            decls = decls(222, [], [], [], [], [], [], {}, {})
            &goal.trait_id = Debug"#
    ]]);
}

#[test]
fn test_exists_not_local() {
    test_prove(
        Decls::empty(),
        term("exists<ty T> {} => {@IsLocal(Debug(T))}"),
    )
    .assert_ok(expect!["{Constraints { env: Env { variables: [?ty_1], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: false, substitution: {} }}"])
}
