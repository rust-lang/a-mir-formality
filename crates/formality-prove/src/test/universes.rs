use expect_test::expect;
use formality_macros::test;
use formality_types::parse::term;

use crate::program::Program;

/// Simple example program consisting only of two trait declarations.
fn program() -> Program {
    Program {
        max_size: Program::DEFAULT_MAX_SIZE,
        trait_decls: vec![],
        impl_decls: vec![],
        alias_eq_decls: vec![],
        alias_bound_decls: vec![],
    }
}

/// There is no U that is equal to all T.
#[test]
fn exists_u_for_t() {
    let constraints = super::test_prove(program(), term("<ty U> ({}, {for<ty T> equals(T, U)})"));
    expect![[r#"
            {}
        "#]]
    .assert_debug_eq(&constraints);
}
