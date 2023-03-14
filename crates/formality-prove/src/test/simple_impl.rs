use expect_test::expect;
use formality_macros::test;
use formality_types::{grammar::Wc, parse::term};

use crate::{decls::Decls, prove::prove};

/// Simple example decls consisting only of two trait declarations.
fn decls() -> Decls {
    Decls {
        max_size: Decls::DEFAULT_MAX_SIZE,
        trait_decls: vec![term("trait Debug<ty Self> where {}")],
        impl_decls: vec![
            term("impl<ty T> Debug(Vec<T>) where {Debug(T)}"),
            term("impl<> Debug(u32) where {}"),
        ],
        alias_eq_decls: vec![],
        alias_bound_decls: vec![],
    }
}

#[test]
fn vec_u32_debug() {
    let goal: Wc = term("Debug(Vec<u32>)");
    let constraints = prove(decls(), (), (), goal);
    expect![[r#"
        {
            Constraints {
                env: Env {
                    variables: [],
                },
                known_true: true,
                substitution: {},
            },
        }
    "#]]
    .assert_debug_eq(&constraints);
}

#[test]
fn vec_vec_u32_debug() {
    let goal: Wc = term("Debug(Vec<Vec<u32>>)");
    let constraints = prove(decls(), (), (), goal);
    expect![[r#"
        {
            Constraints {
                env: Env {
                    variables: [],
                },
                known_true: true,
                substitution: {},
            },
        }
    "#]]
    .assert_debug_eq(&constraints);
}
