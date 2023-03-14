use expect_test::expect;
use formality_macros::test;
use formality_types::parse::term;

use crate::decls::Decls;

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Decls {
    Decls {
        max_size: 10,
        trait_decls: vec![term("trait Debug<ty Self> where {}")],
        impl_decls: vec![term("impl<ty T> Debug(Vec<T>) where {Debug(T)}")],
        ..Decls::empty()
    }
}

/// There is no U that is equal to all T.
#[test]
fn expanding() {
    let constraints = super::test_prove(decls(), term("exists<ty T> {} => {Debug(T)}"));
    expect![[r#"
        {
            Constraints {
                env: Env {
                    variables: [
                        ?ty_0,
                    ],
                },
                known_true: false,
                substitution: {},
            },
        }
    "#]]
    .assert_debug_eq(&constraints);
}
