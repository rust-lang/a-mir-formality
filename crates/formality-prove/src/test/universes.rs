use expect_test::expect;
use formality_macros::test;
use formality_types::rust::term;

use crate::decls::Decls;

use crate::test_util::test_prove;

/// There is no U that is equal to all T.
#[test]
fn exists_u_for_t() {
    let decls = Decls::empty();
    test_prove(decls, term("exists<ty U> {} => {for<ty T> T = U}")).assert_err(expect![[r#"
        the rule "existential-nonvar" at (prove_eq.rs) failed because
          pattern `None` did not match value `Some(!ty_1)`

        the rule "existential-universal" at (prove_eq.rs) failed because
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

    test_prove(decls, term("{} => {for<ty T> Test(T, T)}")).assert_ok(expect!["{Constraints { env: Env { variables: [], bias: Soundness, pending: [], allow_pending_outlives: false }, known_true: true, substitution: {} }}"]);
}
