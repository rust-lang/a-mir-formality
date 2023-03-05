use expect_test::expect;
use formality_macros::test;
use formality_types::parse::term;

use crate::program::Program;

use super::test_prove;

/// Simple example program consisting only of two trait declarations.
fn program() -> Program {
    Program {
        max_size: Program::DEFAULT_MAX_SIZE,
        trait_decls: vec![term("trait Foo<ty Self> where {}")],
        impl_decls: vec![term("impl<ty T> Foo(Vec<T>) where {}")],
        alias_eq_decls: vec![],
        alias_bound_decls: vec![],
    }
}

/// Test that `exists<T> Foo(U)` yields `U = Vec<X>` for some fresh `X`
#[test]
fn exists_u_for_t() {
    let constraints = test_prove(program(), term("<ty U> ({}, {Foo(U)})"));
    expect![[r#"
        {
            <ty> Constraints { result: (), known_true: true, substitution: Substitution { map: {?ty_0: (rigid (adt Vec) ^ty0_0)} } },
        }
    "#]]
        .assert_debug_eq(&constraints);
}
