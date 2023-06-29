use expect_test::expect;
use formality_macros::test;
use formality_types::parse::term;

use crate::decls::Decls;

use crate::test_util::test_prove;

/// There is no U that is equal to all T.
#[test]
fn exists_u_for_t() {
    let decls = Decls::empty();
    let constraints = test_prove(decls, term("exists<ty U> {} => {for<ty T> T = U}"));
    expect![[r#"
            {}
        "#]]
    .assert_debug_eq(&constraints);
}

/// There is U that is equal to some T.
#[test]
fn for_t_exists_u() {
    let decls = Decls {
        trait_decls: vec![term("safe trait Test<ty Self, ty T> where {}")],
        impl_decls: vec![term("safe impl<ty X, ty Y> Test(X, Y) where {X = Y}")],
        ..Decls::empty()
    };

    let constraints = test_prove(decls, term("{} => {for<ty T> Test(T, T)}"));
    expect![[r#"
        {
            Constraints {
                env: Env {
                    variables: [],
                    coherence_mode: false,
                },
                known_true: true,
                substitution: {},
            },
        }
    "#]]
    .assert_debug_eq(&constraints);
}
