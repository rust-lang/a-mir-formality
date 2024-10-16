use expect_test::expect;
use formality_macros::test;
use formality_types::rust::term;

use crate::decls::Decls;

use crate::test_util::test_prove;

#[test]
fn test_forall_not_local() {
    test_prove(
        Decls::empty(),
        term("{} => {for<ty T> @IsLocal(Debug(T))}"),
    ).assert_err(
    expect![[r#"
        judgment `prove { goal: {for <ty> @ IsLocal(Debug(^ty0_0))}, assumptions: {}, env: Env { variables: [], bias: Soundness }, decls: decls(222, [], [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
          failed at (src/file.rs:LL:CC) because
            judgment `prove_wc_list { goal: {for <ty> @ IsLocal(Debug(^ty0_0))}, assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                judgment `prove_wc { goal: for <ty> @ IsLocal(Debug(^ty0_0)), assumptions: {}, env: Env { variables: [], bias: Soundness } }` failed at the following rule(s):
                  the rule "forall" failed at step #2 (src/file.rs:LL:CC) because
                    judgment `prove_wc { goal: @ IsLocal(Debug(!ty_1)), assumptions: {}, env: Env { variables: [!ty_1], bias: Soundness } }` failed at the following rule(s):
                      the rule "trait ref is local" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `is_local_trait_ref { goal: Debug(!ty_1), assumptions: {}, env: Env { variables: [!ty_1], bias: Soundness } }` failed at the following rule(s):
                          the rule "local trait" failed at step #0 (src/file.rs:LL:CC) because
                            condition evaluted to false: `decls.is_local_trait_id(&goal.trait_id)`
                              decls = decls(222, [], [], [], [], [], [], [], {}, {})
                              &goal.trait_id = Debug"#]]);
}

#[test]
fn test_exists_not_local() {
    test_prove(
        Decls::empty(),
        term("exists<ty T> {} => {@IsLocal(Debug(T))}"),
    )
    .assert_ok(expect![[r#"
        {
          Constraints { env: Env { variables: [?ty_1], bias: Soundness }, known_true: false, substitution: {} },
        }
    "#]])
}
