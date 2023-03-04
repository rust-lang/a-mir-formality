use expect_test::expect;
use formality_macros::test;
use formality_types::parse::term;

use crate::program::Program;

/// There is no U that is equal to all T.
#[test]
fn exists_u_for_t() {
    let program = Program::empty();
    let constraints = super::test_prove(program, term("<ty U> ({}, {for<ty T> T = U})"));
    expect![[r#"
            {}
        "#]]
    .assert_debug_eq(&constraints);
}

/// There is U that is equal to some T.
#[test]
fn for_t_exists_u() {
    let program = Program {
        max_size: Program::DEFAULT_MAX_SIZE,
        trait_decls: vec![term("trait Test<ty Self, ty T> where {}")],
        impl_decls: vec![term("impl<ty X, ty Y> Test(X, Y) where {X = Y}")],
        alias_eq_decls: vec![],
        alias_bound_decls: vec![],
    };

    let constraints = super::test_prove(program, term("<> ({}, {for<ty T> Test(T, T)})"));
    expect![[r#"
        {
            <> Constraints { known_true: true, substitution: Substitution { map: {} } },
        }
    "#]]
    .assert_debug_eq(&constraints);
}
