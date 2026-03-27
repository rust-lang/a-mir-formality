use crate::rust::term;
use expect_test::expect;
use formality_macros::test;

use crate::prove::prove::decls::Decls;

use crate::prove::prove::test_util::test_prove;

/// There is no U that is equal to all T.
#[test]
fn exists_u_for_t() {
    let decls = Decls::empty();
    test_prove(decls, term("exists<U> {} => {for<T> T = U}")).assert_err(expect![[r#"
        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:19:1: no applicable rules for prove_normalize { p: !ty_1, assumptions: {}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }

        the rule "existential-nonvar" at (prove_eq.rs) failed because
          pattern `None` did not match value `Some(!ty_1)`

        the rule "existential-universal" at (prove_eq.rs) failed because
          condition evaluated to false: `env.universe(p) < env.universe(v)`

        crates/formality-rust/src/prove/prove/prove/prove_normalize.rs:19:1: no applicable rules for prove_normalize { p: ?ty_0, assumptions: {}, env: Env { variables: [?ty_0, !ty_1], bias: Soundness, pending: [], allow_pending_outlives: false } }"#]]);
}

/// There is U that is equal to some T.
#[test]
fn for_t_exists_u() {
    let decls = Decls {
        crates: Decls::program_from_items(vec![
            term("trait Test<T> where {}"),
            term("impl<X> Test<X> for X {}"),
        ]),
        ..Decls::empty()
    };

    test_prove(decls, term("{} => {for<T> Test(T, T)}")).assert_ok(expect!["{Constraints { env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}
