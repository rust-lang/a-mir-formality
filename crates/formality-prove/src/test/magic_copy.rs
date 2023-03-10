use expect_test::expect;
use formality_macros::test;
use formality_types::parse::term;

use crate::program::Program;

use super::test_prove;

/// Simple example program consisting only of two trait declarations.
fn program() -> Program {
    Program {
        max_size: Program::DEFAULT_MAX_SIZE,
        trait_decls: vec![
            term("trait Copy<ty Self> where {}"),
            term("trait Magic<ty Self> where {Copy(Self)}"),
        ],
        impl_decls: vec![
            term("impl<ty T> Magic(T) where {Magic(T)}"),
            term("impl<> Copy(u32) where {}"),
        ],
        alias_eq_decls: vec![],
        alias_bound_decls: vec![],
    }
}

#[test]
fn all_t_not_magic() {
    let constraints = test_prove(program(), term("<> ({}, {for<ty T> Magic(T)})"));
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
