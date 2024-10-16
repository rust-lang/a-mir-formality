use expect_test::expect;
use formality_macros::test;
use formality_types::rust::term;

use crate::decls::Decls;

use crate::test_util::test_prove;

/// There is no U that is equal to all T.
#[test]
fn exists_u_for_t() {
    let decls = Decls::empty();
    test_prove(decls, term("exists<ty U> {} => {for<ty T> T = U}")).assert_err(
    expect![[r#"
        judgment `prove { goal: {for <ty> ^ty0_0 = ?ty_0}, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness }, decls: decls(222, [], [], [], [], [], [], [], {}, {}) }` failed at the following rule(s):
          failed at (src/file.rs:LL:CC) because
            judgment `prove_wc_list { goal: {for <ty> ^ty0_0 = ?ty_0}, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness } }` failed at the following rule(s):
              the rule "some" failed at step #0 (src/file.rs:LL:CC) because
                judgment `prove_wc { goal: for <ty> ^ty0_0 = ?ty_0, assumptions: {}, env: Env { variables: [?ty_0], bias: Soundness } }` failed at the following rule(s):
                  the rule "forall" failed at step #2 (src/file.rs:LL:CC) because
                    judgment `prove_wc { goal: !ty_1 = ?ty_0, assumptions: {}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness } }` failed at the following rule(s):
                      the rule "eq" failed at step #0 (src/file.rs:LL:CC) because
                        judgment `prove_eq { a: !ty_1, b: ?ty_0, assumptions: {}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness } }` failed at the following rule(s):
                          the rule "symmetric" failed at step #0 (src/file.rs:LL:CC) because
                            judgment `prove_eq { a: ?ty_0, b: !ty_1, assumptions: {}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness } }` failed at the following rule(s):
                              the rule "existential" failed at step #0 (src/file.rs:LL:CC) because
                                judgment `prove_existential_var_eq { v: ?ty_0, b: !ty_1, assumptions: {}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness } }` failed at the following rule(s):
                                  the rule "existential-nonvar" failed at step #0 (src/file.rs:LL:CC) because
                                    pattern `None` did not match value `Some(!ty_1)`
                                  the rule "existential-universal" failed at step #0 (src/file.rs:LL:CC) because
                                    condition evaluted to false: `env.universe(p) < env.universe(v)`"#]]);
}

/// There is U that is equal to some T.
#[test]
fn for_t_exists_u() {
    let decls = Decls {
        trait_decls: vec![term("trait Test<ty Self, ty T> where {}")],
        impl_decls: vec![term("impl<ty X, ty Y> Test(X, Y) where {X = Y}")],
        ..Decls::empty()
    };

    test_prove(decls, term("{} => {for<ty T> Test(T, T)}")).assert_ok(expect![[r#"
        {
          Constraints { env: Env { variables: [], bias: Soundness }, known_true: true, substitution: {} },
        }
    "#]]);
}
